package com.tinylabproductions.amazon_appstore_automator.app

import java.util.concurrent.{Executors, TimeUnit}

import com.tinylabproductions.amazon_appstore_automator.{Cfg, PackageNameToAppIdMapping, ReleaseNotes, Releases}
import com.tinylabproductions.amazon_appstore_automator.util.Log._
import com.tinylabproductions.amazon_appstore_automator.util.Retries

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
    releases.v.zipWithIndex.foreach { case (release, idx) =>
      executor.submit(new Runnable {
        override def run(): Unit = {
          val name = s"Uploading $release (${idx + 1}/${releases.v.size})"
          withSessionRetries(name) { app =>
            info(name)
            mapping.mapping.get(release.publishInfo.packageName) match {
              case Some(appId) => app.updateApp(appId, release.apkPath, releaseNotes, params)
              case None => err(s"Can't find app id for $release, skipping!")
            }
          }
        }
      })
    }
    executor.awaitTermination(7, TimeUnit.DAYS)
    info("Done uploading releases.")
  }
}
