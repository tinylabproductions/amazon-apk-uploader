package com.tinylabproductions.amazon_appstore_automator.app


import java.nio.file.{Files, Path}

import com.tinylabproductions.amazon_appstore_automator._
import com.tinylabproductions.amazon_appstore_automator.util.Log
import org.scalatest.concurrent.Eventually
import org.scalatest.selenium.Chrome
import org.scalatest.time.{Milliseconds, Seconds, Span}
import play.api.libs.json.Json

import scala.annotation.tailrec
import scala.util.matching.Regex

sealed trait AppUpdateStatus
object AppUpdateStatus {
  case object Success extends AppUpdateStatus
  case object AlreadySubmitting extends AppUpdateStatus {
    override def toString = "Could not submit app because it is already currently submitting."
  }
  case class Error(s: String) extends AppUpdateStatus
}

class App extends Chrome with WebBrowserOps with UpdateMappings with Eventually {
  import com.tinylabproductions.amazon_appstore_automator.util.Log._

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
  def goToContentRating(): Unit = goTo("Content Rating tab", id("header_nav_rating_a"))
  val editButton: IdQuery = id("edit_button")

  def upcomingVersionLinkElem: Option[Element] = find(id("upcoming_version_a"))
  def currentVersionUnderReview: Boolean =
    find(cssSelector("#current_version_a .versionStatus")).exists(_.text.trim == "(Under Review)")

  def signIn(credentials: Credentials): Boolean = {
    def isLoggedIn = find(id("appsandservices_myapps")).isDefined

    if (!isLoggedIn) {
      go to "https://developer.amazon.com/myapps.html"
      emailField(id("ap_email")).value = credentials.username
      new PasswordField(findOrDie(id("ap_password")).underlying).value = credentials.password
      click on id("signInSubmit")
    }

    isLoggedIn
  }

  def getLatestMapping(
    cfg: Cfg, releases: Releases, initialMapping: PackageNameToAppIdMapping
  ): PackageNameToAppIdMapping = {
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
    val unknownPackages =
      releases.v.map(_.publishInfo.packageName).filterNot(initialMapping.mapping.contains)
    val mapping =
      if (unknownPackages.isEmpty) initialMapping
      else {
        info(s"Have unknown ${unknownPackages.size} android packages, need to collect amazon app ids:")
        unknownPackages.foreach { pkg =>
          info(s"- ${pkg.s}")
        }
        doUpdateMapping()
      }

    mapping
  }

  def appUrl(appId: AmazonAppId): String =
    s"https://developer.amazon.com/application/general/${appId.s}/detail.html"

  def updateApp(
    appId: AmazonAppId, apk: Path, releaseNotes: ReleaseNotes, params: UpdateAppParams
  ): AppUpdateStatus = {
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
    (find(editButton), find(id("cancel_app_button"))) match {
      case (None, Some(_)) =>
        info(s"App already submitting, skipping $appId for $apk.")
        AppUpdateStatus.AlreadySubmitting
      case (None, None) =>
        val error =
          s"Neither edit not cancel submission buttons could be found in binary files page!\n"
          s"Cannot continue uploading, skipping $appId for $apk."
        err(error)
        AppUpdateStatus.Error(error)
      case (Some(edit), _) =>
        // Edit
        click on edit
        removeBinaryFiles()

        // Upload file
        val box = eventually { findOrDie(id("appBinary"), "upload binary box") }
        Log.info(s"Uploading $apk to $box")
        box.underlying.sendKeys(apk.toRealPath().toString)

        find(id("itemSection.errors")) match {
          case Some(error) =>
            val errorS = s"Uploading $appId APK resulted in an error, skipping $apk:\n ${error.text}"
            err(errorS)
            AppUpdateStatus.Error(errorS)
          case None =>
            // Save
            click on id("submit_button")

            goToReleaseNotes()
            click on editButton

            eventually { textArea(id("releaseNotes")).value = releaseNotes.s }
            // Save
            click on id("submit_button")

            params.appDirectedUnderAge13.foreach { directedFor =>
              goToContentRating()
              click on editButton
              eventually { click on id(s"child_directed_${if (directedFor) "Yes" else "No"}") }
              click on id("submit_button")
            }

            // Submit to store
            if (params.submitApp) click on id("submit_app_button")
            AppUpdateStatus.Success
        }
    }
  }
}
