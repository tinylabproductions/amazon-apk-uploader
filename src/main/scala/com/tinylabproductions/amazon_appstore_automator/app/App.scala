package com.tinylabproductions.amazon_appstore_automator.app


import java.nio.file.{Files, Path}
import java.time.LocalDateTime

import com.tinylabproductions.amazon_appstore_automator._
import org.scalatest.concurrent.Eventually
import org.scalatest.selenium.Chrome
import org.scalatest.time.{Milliseconds, Seconds, Span}
import play.api.libs.json.Json

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.matching.Regex

object App extends App
trait App extends Chrome with WebBrowserOps with UpdateMappings with Eventually {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(
    timeout = Span(10, Seconds),
    interval = Span(100, Milliseconds)
  )

  val AppLinkRe: Regex =
    """^https://developer\.amazon\.com/application/general/(\w+)/detail\.html$""".r

  def href(elem: Element, name: String): String =
    elem.attribute("href").getOrElse(sys.error(s"Unable to retrieve href for $name: $elem"))

  def goTo(name: String, query: Query): Unit = {
    val elem = findOrDie(query, name)
    go to href(elem, name)
  }

  def goToBinaryFiles(): Unit = goTo("Binary Files tab", id("header_nav_binary_a"))
  def goToReleaseNotes(): Unit = goTo("Release Notes tab", id("header_nav_releasenotes_a"))
  def upcomingVersionLinkElem: Option[Element] = find(id("upcoming_version_a"))
  def currentVersionUnderReview: Boolean =
    find(cssSelector("#current_version_a .versionStatus")).exists(_.text.trim == "(Under Review)")

  def err(o: Any): Unit = Console.err.println(s"[${LocalDateTime.now()}] !!! $o")
  def warn(o: Any): Unit = println(s"[${LocalDateTime.now()}] *** $o")
  def info(o: Any): Unit = println(s"[${LocalDateTime.now()}] $o")

  def work(
    cfg: Cfg, releaseNotes: ReleaseNotes, releases: Releases,
    initialMapping: PackageNameToAppIdMapping, submitApp: Boolean
  ): Unit = {
    go to "https://developer.amazon.com/myapps.html"
    def isLoggedIn = find(id("appsandservices_myapps")).isDefined

    if (!isLoggedIn) {
      for {
        username <- cfg.credentials.username
        field <- find(id("ap_email")).map(e => new EmailField(e.underlying))
      } field.value = username
      for {
        password <- cfg.credentials.password
        field <- find(id("ap_password")).map(e => new PasswordField(e.underlying))
      } field.value = password

      if (cfg.credentials.username.isDefined && cfg.credentials.password.isDefined) {
        click on id("signInSubmit")
      }
    }

    while (!isLoggedIn) {
      val timeout = 1.second
      info(s"Not logged in, please log in. Checking again in $timeout")
      Thread.sleep(timeout.toMillis)
    }

    def doUpdateMapping(): PackageNameToAppIdMapping = {
      info("Updating mapping...")
      val result = updateMapping(
        cfg.amazonAppSkuMustMatchAndroidPackageName, cfg.ignoredAppIds, initialMapping
      )
      Files.write(cfg.mappingFilePath, Json.toBytes(Json.toJson(result.mapping)))
      info(
        s"Mapping updated. " +
          s"Old: ${initialMapping.mapping.size} entries, new: ${result.mapping.mapping.size} entries."
      )
      if (result.warnings.nonEmpty) {
        warn("### Found warnings:")
        result.warnings.foreach { warning =>
          warn(s" - $warning")
        }
      }
      if (result.errors.nonEmpty) {
        err("### Found errors:")
        result.errors.foreach { error =>
          err(s" - $error")
        }

        err("Aborting.")
        sys.exit(2)
      }
      result.mapping
    }

    // Do we have enough mapping data so we could proceed?
    val mappingSufficient =
      releases.v.map(_.publishInfo.packageName).forall(initialMapping.mapping.contains)
    val mapping = if (mappingSufficient) initialMapping else doUpdateMapping()

    info(s"Uploading ${releases.v.size} releases...")
    releases.v.zipWithIndex.foreach { case (release, idx) =>
      info(s"Uploading $release (${idx + 1}/${releases.v.size})")
      mapping.mapping.get(release.publishInfo.packageName) match {
        case Some(appId) => updateApp(appId, release.apkPath, releaseNotes, submitApp)
        case None => err(s"Can't find app id for $release, skipping!")
      }
    }
    info("Done uploading releases.")
  }

  def appUrl(appId: AmazonAppId): String =
    s"https://developer.amazon.com/application/general/${appId.s}/detail.html"

  def updateApp(
    appId: AmazonAppId, apk: Path, releaseNotes: ReleaseNotes, submitApp: Boolean
  ): Unit = {
    // Old App IDs still work, they just say that we are looking at an archived version of an
    // app and provide a link to new version cssSelector(".app-status-ARCHIVED a").
    //
    // However for our functionality that is fine, as we just need the upcoming version link.
    go to appUrl(appId)

    (upcomingVersionLinkElem, currentVersionUnderReview) match {
      case (None, false) =>
        // Add upcoming version
        eventually { click on findOrDie(id("new_version_a"), "add upcoming version link") }
        eventually { findOrDie(id("post_confirm_form"), "confirmation form").underlying.submit() }
      case (None, true) =>
        err(s"Current version under review for $appId, skipping $apk")
      case (Some(elem), _) =>
        go to href(elem, "upcoming version link")
    }

    val uploadApkFieldQuery = id("appBinary")
    def canUploadNewApk = find(uploadApkFieldQuery).isDefined
    // Remove current binary files
    @tailrec def removeBinaryFiles(): Unit = {
      def findRemoveAssetButton = find(cssSelector("a[id^=remove_asset_button_]"))
      while (!canUploadNewApk && findRemoveAssetButton.isEmpty) {
        // Wait until one or other shows up.
        Thread.sleep(50)
      }

      findRemoveAssetButton match {
        case Some(a) =>
          info(s"Removing binary file: $a")
          click on a
          click on id("floatingconfirm-ok")
          removeBinaryFiles()
        case None =>
          info("All binary files removed.")
      }
    }

    // Binary Files
    goToBinaryFiles()
    (find(id("edit_button")), find(id("cancel_app_button"))) match {
      case (None, Some(_)) =>
        info("App already submitting, skipping.")
      case (None, None) =>
        err("Neither edit not cancel submission buttons could be found in binary files page!")
        err(s"Cannot continue uploading, skipping $appId for $apk.")
      case (Some(edit), _) =>
        // Edit
        click on edit
        removeBinaryFiles()

        // Upload file
        val box = eventually { findOrDie(id("appBinary"), "upload binary box") }
        info(s"Uploading $apk to $box")
        box.underlying.sendKeys(apk.toRealPath().toString)

        find(id("itemSection.errors")) match {
          case Some(error) =>
            err(s"Uploading $appId APK resulted in an error, skipping $apk!")
            err(error.text)
          case None =>
            // Save
            click on id("submit_button")

            goToReleaseNotes()
            click on id("edit_button")
            eventually { textArea(id("releaseNotes")).value = releaseNotes.s }
            // Save
            click on id("submit_button")

            // Submit to store
            if (submitApp) click on id("submit_app_button")
        }
    }
  }
}
