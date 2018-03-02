package com.tinylabproductions.amazon_appstore_automator.app

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.Executors

import cats.instances.all._
import cats.syntax.all._
import com.softwaremill.quicklens._
import com.tinylabproductions.amazon_appstore_automator.util.Log._
import com.tinylabproductions.amazon_appstore_automator.util.{Pool, Retries}
import com.tinylabproductions.amazon_appstore_automator.{AmazonAppId, Cfg, HasErrors, HasWarnings, LogLevel, PackageNameToAppIdMapping, ReleaseNotes, Releases, ThrowableExts}
import play.api.libs.json.Json

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

class AppUploader(credentials: Credentials) {
  private[this] def destroyA(app: App): Unit = app.webDriver.close()
  private[this] val sessions = new Pool[App](
    create = () => {
      val app = new App
      if (!app.signIn(credentials)) {
        throw new Exception("Can't sign in with provided credentials!")
      }
      app
    },
    destroy = destroyA
  )

  def withSessionRetries[A](
    name: String, retries: Int = 5
  )(f: App => A): Try[A] =
    Try(Retries.withRetries(name, retries)(sessions.withSession(f)))

  def showWarnings(warnings: HasWarnings): Unit = {
    if (warnings.warnings.nonEmpty) {
      warn("### Found warnings:")
      warnings.warnings.foreach { warning =>
        warn(s" - $warning")
      }
    }
  }

  def showErrors(errors: HasErrors): Unit = {
    if (errors.errors.nonEmpty) {
      err("### Found errors:")
      errors.errors.foreach { error =>
        err(s" - $error")
      }

      err("Aborting.")
      sys.exit(2)
    }
  }

  def apply(
    cfg: Cfg, releaseNotes: ReleaseNotes, releases: Releases,
    initialMapping: PackageNameToAppIdMapping, params: UpdateAppParams
  ): Unit = {
    val mapping = Await.result(
      getLatestMapping(cfg, releases, initialMapping, params.forceUpdateMapping),
      7.days
    )

    sessions.destroyN(cfg.scrapeParalellism - cfg.uploadParalellism)
    info(s"Uploading ${releases.v.size} releases...")
    val results = Await.result(atMost = 7.days, awaitable = {
      executorWithContext(cfg.uploadParalellism) { implicit ec =>
        Future.sequence(releases.v.zipWithIndex.map { case (release, idx) =>
          Future {
            val name = s"Uploading $release (${idx + 1}/${releases.v.size})"
            val res = withSessionRetries(name) { app =>
              info(name)
              mapping.mapping.get(release.publishInfo.packageName) match {
                case Some(appId) =>
                  app.updateApp(appId, release.apkPath, releaseNotes, params)
                case None =>
                  val error = s"Can't find app id for $release, skipping!"
                  err(error)
                  AppUpdateStatus.Error(error)
              }
            }
            release -> res
          }
        })
      }
    })
    info("Done uploading releases.")
    sessions.destroyAll()

    val (errors, successful) = results.map {
      case (release, util.Success(AppUpdateStatus.Success)) =>
        Right(release)
      case (release, util.Success(status)) =>
        Left(release, status)
      case (release, util.Failure(t)) =>
        Left(release, AppUpdateStatus.Error(s"Update failed with an exception: ${t.asString}"))
    }.partitionEithers

    if (successful.nonEmpty) {
      info(s"Successful (${successful.size}/${results.size}):")
      successful.foreach { release =>
        info(s"- $release")
      }
    }
    if (errors.nonEmpty) {
      info(s"!!! Failed (${errors.size}/${results.size}):")
      errors.foreach { case (release, status) =>
        info(s"!!! - $release")
        info(s"!!!   $status")
      }
    }
  }

  def getLatestMapping(
    cfg: Cfg, releases: Releases, initialMapping: PackageNameToAppIdMapping, forceUpdate: Boolean
  ): Future[PackageNameToAppIdMapping] = {
    def doUpdateMapping(): Future[PackageNameToAppIdMapping] = {
      import scala.concurrent.ExecutionContext.Implicits.global
      info("Updating mapping...")
      for {
        scrapedAppIds <- fetchAllAppIds(cfg.scrapeParalellism)
        _ = showErrors(scrapedAppIds)
        unknownIds = scrapedAppIds.ids -- initialMapping.mapping.values -- cfg.ignoredAppIds
        mapping <-
          scrapeUnknownIds(
            cfg.scrapeParalellism, cfg.amazonAppSkuMustMatchAndroidPackageName, unknownIds
          ).map { result =>
            val newMapping = initialMapping ++ result.map
            Files.write(
              cfg.mappingFilePath,
              Json.prettyPrint(Json.toJson(newMapping)).getBytes(StandardCharsets.UTF_8)
            )
            info(
              s"Mapping updated. " +
              s"Old: ${initialMapping.mapping.size} entries, new: ${result.map.mapping.size} entries."
            )
            showWarnings(result)
            showErrors(result)
            newMapping
          }
      } yield mapping
    }

    // Do we have enough mapping data so we could proceed?
    val unknownPackages =
      releases.v.map(_.publishInfo.packageName).filterNot(initialMapping.mapping.contains)
    val mappingF =
      if (unknownPackages.isEmpty && !forceUpdate)
        Future.successful(initialMapping)
      else {
        info(s"Have unknown ${unknownPackages.size} android packages, need to collect amazon app ids:")
        unknownPackages.foreach { pkg =>
          info(s"- ${pkg.s}")
        }
        doUpdateMapping()
      }

    mappingF
  }

  def fetchAllAppIds(parallelism: Int): Future[FetchAppIdsResult] = {
    def fetch(app: App, startingPage: Int): FetchAppIdsResult = {
      @tailrec def rec(currentPage: Int, currentResult: FetchAppIdsResult): FetchAppIdsResult = {
        val res = app.scrapeIds(currentPage)
        if (res.isEmpty) currentResult
        else rec(currentPage + parallelism, currentResult combine res)
      }

      rec(startingPage, FetchAppIdsResult.empty)
    }

    executorWithContext(parallelism) { implicit ec =>
      Future.sequence((1 to parallelism).toVector.map { startingPage =>
        Future { sessions.withSession(fetch(_, startingPage)) }
      }).map(_.combineAll)
    }
  }

  def scrapeUnknownIds(
    parallelism: Int,
    onSkuMismatches: LogLevel,
    unknownIds: Set[AmazonAppId]
  ): Future[ScrapeAppIds] = {
    executorWithContext(parallelism) { implicit ec =>
      Future.sequence(unknownIds.toVector.zipWithIndex.map { case (appId, idx) =>
        Future {
          info(s"Retrieving details for $appId ${idx + 1}/${unknownIds.size}...")
          sessions.withSession(_.scrapeAppId(onSkuMismatches, appId))
        }
      }).map { result =>
        val (errors, scrapedAppIds) = result.partitionEithers
        val ids = scrapedAppIds.map(_.toIds).fold(ScrapeAppIds.empty)(_ ++ _)
        ids.modify(_.errors).using(_ ++ errors)
      }
    }
  }

  def executorWithContext[A](parallelism: Int)(f: ExecutionContext => Future[A]): Future[A] = {
    val executor = Executors.newFixedThreadPool(parallelism)
    implicit val context: ExecutionContext = ExecutionContext.fromExecutor(executor)
    val future = f(context)
    future.onComplete(_ => executor.shutdown())
    future
  }
}
