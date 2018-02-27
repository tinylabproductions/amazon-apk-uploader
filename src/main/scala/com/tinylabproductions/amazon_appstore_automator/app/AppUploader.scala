package com.tinylabproductions.amazon_appstore_automator.app

import java.util.concurrent.Executors

import com.tinylabproductions.amazon_appstore_automator.util.Log._
import com.tinylabproductions.amazon_appstore_automator.util.Retries
import com.tinylabproductions.amazon_appstore_automator.{Cfg, PackageNameToAppIdMapping, ReleaseNotes, Releases, ThrowableExts}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object AppUploader {
  private[this] val globalSession = ThreadLocal.withInitial[Option[App]](() => Option.empty[App])

  def withSession[A](
    closeSessionOnFinish: Boolean = false
  )(f: App => A)(implicit credentials: Credentials): Try[A] = {
    def closeSession(): Unit = {
      globalSession.get().foreach { session =>
        session.webDriver.close()
        globalSession.set(None)
      }
    }

    try {
      if (globalSession.get().isEmpty) globalSession.set(Some(new App))
      val session = globalSession.get.get
      if (!session.signIn(credentials)) {
        throw new Exception("Can't sign in with provided credentials!")
      }

      val result = f(session)
      if (closeSessionOnFinish) closeSession()
      Success(result)
    }
    catch {
      case NonFatal(t) =>
        closeSession()
        Failure(t)
    }
  }

  def withSessionRetries[A](
    name: String, retries: Int = 5, closeSessionOnFinish: Boolean = false
  )(f: App => A)(
    implicit credentials: Credentials
  ): Try[A] =
    Retries.withRetries(name, retries)(withSession(closeSessionOnFinish)(f))

  def apply(
    cfg: Cfg, releaseNotes: ReleaseNotes, releases: Releases,
    initialMapping: PackageNameToAppIdMapping, params: UpdateAppParams
  )(implicit credentials: Credentials): Unit = {
    val mapping = withSessionRetries("get latest mapping", closeSessionOnFinish = true) { app =>
      app.getLatestMapping(cfg, releases, initialMapping)
    }.get

    info(s"Uploading ${releases.v.size} releases...")
    val executor = Executors.newFixedThreadPool(cfg.uploadParalellism)
    implicit val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)
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
    val results = Await.result(resultsF, 7.days)
    info("Done uploading releases.")
    
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
      err(s"Failed (${errors.size}/${results.size}):")
      errors.foreach { case (release, status) =>
        err(s"- $release")
        err(s"  $status")
      }
    }
  }
}
