package com.tinylabproductions.amazon_appstore_automator.app

import java.util.concurrent.Executors

import com.tinylabproductions.amazon_appstore_automator.util.Log._
import com.tinylabproductions.amazon_appstore_automator.util.{Pool, Retries}
import com.tinylabproductions.amazon_appstore_automator.{AmazonAppId, Cfg, HasErrors, HasWarnings, LogLevel, PackageNameToAppIdMapping, ReleaseNotes, Releases, ThrowableExts}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import cats.instances.all._
import cats.syntax.all._
import com.softwaremill.quicklens._

object AppUploader {
  private[this] def destroyA(app: App): Unit = app.webDriver.close()
  private[this] val sessions = new Pool[App](
    create = () => new App,
    destroy = destroyA
  )

  def withSession[A](
    closeSessionOnFinish: Boolean = false
  )(f: App => A)(implicit credentials: Credentials): Try[A] = {
    val app = sessions.borrow()
    try {
      if (!app.signIn(credentials)) {
        throw new Exception("Can't sign in with provided credentials!")
      }

      val result = f(app)
      if (closeSessionOnFinish) destroyA(app)
      else sessions.release(app)
      Success(result)
    }
    catch {
      case NonFatal(t) =>
        destroyA(app)
        Failure(t)
    }
  }

  def withSessionRetries[A](
    name: String, retries: Int = 5, closeSessionOnFinish: Boolean = false
  )(f: App => A)(
    implicit credentials: Credentials
  ): Try[A] =
    Retries.withRetries(name, retries)(withSession(closeSessionOnFinish)(f))

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
  )(implicit credentials: Credentials): Unit = {
    val scrapedAppIds = fetchAllAppIds(cfg.scrapeParalellism)
    showErrors(scrapedAppIds)
    val unknownIds = scrapedAppIds.ids -- initialMapping.mapping.values -- cfg.ignoredAppIds

    val mapping = withSessionRetries("get latest mapping") { app =>
      app.getLatestMapping(cfg, releases, initialMapping, unknownIds)
    }.get

    sessions.destroyN(cfg.scrapeParalellism - cfg.uploadParalellism)
    info(s"Uploading ${releases.v.size} releases...")
    val results = {
      executorWithContext(cfg.uploadParalellism) { implicit ec =>
        val resultsF = Future.sequence(releases.v.zipWithIndex.map { case (release, idx) =>
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
        Await.result(resultsF, 7.days)
      }
    }
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

  def fetchAllAppIds(parallelism: Int): FetchAppIdsResult = {
    def fetch(app: App, startingPage: Int): FetchAppIdsResult = {
      @tailrec def rec(currentPage: Int, currentResult: FetchAppIdsResult): FetchAppIdsResult = {
        val res = app.scrapeIds(currentPage)
        if (res.isEmpty) currentResult
        else rec(currentPage + parallelism, currentResult combine res)
      }

      rec(startingPage, FetchAppIdsResult.empty)
    }

    val resultF = executorWithContext(parallelism) { implicit ec =>
      Future.sequence((1 to parallelism).toVector.map { startingPage =>
        Future { sessions.withSession(fetch(_, startingPage)) }
      }).map(_.combineAll)
    }
    Await.result(resultF, 7.days)
  }

  def updateMapping(
    parallelism: Int,
    onSkuMismatches: LogLevel,
    unknownIds: Set[AmazonAppId],
    known: PackageNameToAppIdMapping
  ): ScrapeAppIds = {
    executorWithContext(parallelism) { implicit ec =>
      val resultF = Future.sequence(unknownIds.toVector.zipWithIndex.map { case (appId, idx) =>
        Future {
          info(s"Retrieving details for $appId ${idx + 1}/${unknownIds.size}...")
          sessions.withSession(_.scrapeAppId(onSkuMismatches, appId))
        }
      })
      val (errors, scrapedAppIds) = Await.result(resultF, 7.days).partitionEithers
      val ids = scrapedAppIds.map(_.toIds).fold(ScrapeAppIds.empty)(_ ++ _)
      ids.modify(_.errors).using(_ ++ errors)
    }
  }

  def executorWithContext[A](parallelism: Int)(f: ExecutionContext => A): A = {
    val executor = Executors.newFixedThreadPool(parallelism)
    try {
      val context = ExecutionContext.fromExecutor(executor)
      f(context)
    }
    finally {
      executor.shutdown()
    }
  }
}
