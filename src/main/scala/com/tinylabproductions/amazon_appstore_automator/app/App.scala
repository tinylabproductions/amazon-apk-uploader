package com.tinylabproductions.amazon_appstore_automator.app


import java.nio.file.{Files, Path}

import com.tinylabproductions.amazon_appstore_automator._
import org.scalatest.selenium.Chrome
import play.api.libs.json.Json

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.matching.Regex

object App extends App
trait App extends Chrome with WebBrowserOps with UpdateMappings {
  val AppLinkRe: Regex =
    """^https://developer\.amazon\.com/application/general/(\w+)/detail\.html$""".r

  def goToBinaryFiles(): Unit = {
    val name = "Binary Files tab"
    val elem = findOrDie(id("header_nav_binary_a"), name)
    val href =
      elem.attribute("href").getOrElse(sys.error(s"Unable to retrieve href for $name: $elem"))
    go to href
  }

  def err(o: Any): Unit = Console.err.println(s"!!! $o")
  def info(o: Any): Unit = println(o)

  def work(cfg: Cfg): Unit = {
    go to "https://developer.amazon.com/myapps.html"
    def checkIfLoggedIn = find(id("appsandservices_myapps")).isDefined
    while (!checkIfLoggedIn) {
      val timeout = 1.second
      info(s"Not logged in, please log in. Checking again in $timeout")
      Thread.sleep(timeout.toMillis)
    }

    val mapping =
      if (Files.exists(cfg.mappingFilePath))
        Json.parse(Files.readAllBytes(cfg.mappingFilePath)).as[PackageNameToAppIdMapping]
      else
        PackageNameToAppIdMapping.empty
    val result = updateMapping(mapping)
    if (result.errors.nonEmpty) {
      err("### Found errors:")
      result.errors.foreach { error =>
        err(s" - $error")
      }
    }
    Files.write(cfg.mappingFilePath, Json.toBytes(Json.toJson(result.mapping)))
  }

  def appUrl(appId: AmazonAppId): String =
    s"https://developer.amazon.com/application/general/${appId.s}/detail.html"

  def updateApp(appId: AmazonAppId, apk: Path): Unit = {
    go to appUrl(appId)

    find(id("upcoming_version_a")) match {
      case None =>
        // Add upcoming version
        click on findOrDie(id("new_version_a"), "add upcoming version link")
        click on findOrDie(
          id("create_new_app_version_confirmation_confirm"),
          "confirm button for add upcoming version"
        )
      case Some(elem) =>
        click on elem
    }

    // Binary Files
    goToBinaryFiles()

    // Edit
    click on id("edit_button")

    // Remove current binary files
    @tailrec def removeBinaryFiles(): Unit = {
      find(cssSelector("a[id^=remove_asset_button_]")) match {
        case Some(a) =>
          info(s"Removing binary file: $a")
          click on a
          click on id("floatingconfirm-ok")
          removeBinaryFiles()
        case None =>
          info("All binary files removed.")
      }
    }
    removeBinaryFiles()

    // Upload file
    findOrDie(id("appBinary"), "upload binary box").underlying.sendKeys(apk.toRealPath().toString)
  }
}
