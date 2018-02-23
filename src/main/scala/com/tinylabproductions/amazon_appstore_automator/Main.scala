package com.tinylabproductions.amazon_appstore_automator

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import com.tinylabproductions.amazon_appstore_automator.app.App
import com.typesafe.config.ConfigFactory
import configs.Result.{Failure, Success}
import configs.syntax._
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.{FileFilterUtils, TrueFileFilter}
import play.api.libs.json.Json

import scala.collection.JavaConverters._
import scala.util.Try

object Main {
  def readUtf8(p: Path): String = new String(Files.readAllBytes(p), StandardCharsets.UTF_8)

  def main(args: Array[String]): Unit = {
    args match {
      case Array(configFileS, releaseNotesS, releasesPathS) =>
        ConfigFactory.parseFile(new File(configFileS)).extract[Cfg] match {
          case Success(cfg) =>
            val releaseNotes = get(
              Try(ReleaseNotes(readUtf8(Paths.get(releaseNotesS))))
              .toEither.left.map { t =>
                s"Can't read release notes from $releaseNotesS: ${t.asString}"
              }
            )
            val releases = Releases(getS(
              FileUtils.iterateFiles(
                new File(releasesPathS),
                FileFilterUtils.nameFileFilter(cfg.publishInfoJsonFilename),
                TrueFileFilter.TRUE
              ).asScala.map(readReleaseDirectory).sequenceValidations
            ))
            val mapping = get(readMapping(cfg.mappingFilePath))

            println(s"Will deploy ${releases.v.size} releases:")
            releases.v.foreach { release =>
              println(s"- $release")
            }
            println()

            println(s"Using Chrome driver: ${cfg.chromeDriverPath}")
            System.setProperty("webdriver.chrome.driver", cfg.chromeDriverPath.toString)

            io.StdIn.readLine("Press enter to continue...")
            App.work(cfg, releaseNotes, releases, mapping)
          case Failure(errors) =>
            Console.err.println("Error while reading configs:")
            errors.entries.foreach { error =>
              Console.err.println(s"- ${error.messageWithPath}")
            }
            sys.exit(2)
        }
      case _ =>
        Console.err.println("Usage: amazon-appstore-automator app.conf path_to_release_notes path_to_releases")
        Console.err.println()
        Console.err.println("path_to_release_notes:")
        Console.err.println("  file with release notes that will be uploaded to app store")
        Console.err.println()
        Console.err.println("path_to_releases:")
        Console.err.println("  directory with subdirectories that each contains publish_info.json")
        Console.err.println("  (configurable from app.conf) and an .apk to upload")
        Console.err.println()
        Console.err.println("Get Chrome from: https://www.google.com/chrome/")
        Console.err.println("Get Chrome driver from: https://sites.google.com/a/chromium.org/chromedriver/")
        sys.exit(1)
    }
  }

  def get[A](e: Either[String, A]): A = e match {
    case Left(error) =>
      Console.err.println(s"Fatal error: $error")
      sys.exit(2)
    case Right(value) =>
      value
  }

  def getS[A](e: Either[Seq[String], A]): A = e match {
    case Left(errors) =>
      Console.err.println(s"Fatal errors:")
      errors.foreach { error =>
        Console.err.println(s"- $error")
      }
      sys.exit(2)
    case Right(value) =>
      value
  }

  def readMapping(p: Path): Either[String, PackageNameToAppIdMapping] = Try {
    if (Files.exists(p))
      Json.parse(Files.readAllBytes(p)).as[PackageNameToAppIdMapping]
    else
      PackageNameToAppIdMapping.empty
  }.toEither.left.map { t => s"Can't read mappings from $p: ${t.asString}" }

  def readReleaseDirectory(publishInfoFile: File): Either[String, Release] = {
    def publishInfoE =
      Json.parse(Files.readAllBytes(publishInfoFile.toPath))
      .validate[PublishInfo]
      .asEither
      .left.map { err => s"Can't parse $publishInfoFile:\n${err.asString}" }
    def apkPathE = {
      val releaseDir = publishInfoFile.getParentFile
      FileUtils.listFiles(releaseDir, Array("apk"), false).asScala.toVector match {
        case Vector(apk) => Right(apk.toPath)
        case other => Left(s"Found 0 or more than 1 apks in release directory $releaseDir: $other")
      }
    }
    for {
      publishInfo <- publishInfoE
      apkPath <- apkPathE
    } yield Release(apkPath, publishInfo)
  }
}