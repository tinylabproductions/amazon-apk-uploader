package com.tinylabproductions.amazon_appstore_automator

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.tinylabproductions.amazon_appstore_automator.app.{AppUploader, Credentials}
import com.typesafe.config.ConfigFactory
import configs.Result.{Failure, Success}
import configs.syntax._
import org.apache.commons.io.FileUtils
import play.api.libs.json.Json

import scala.collection.JavaConverters._
import scala.util.Try

object Main {
  def readUtf8(p: Path): String = new String(Files.readAllBytes(p), StandardCharsets.UTF_8)

  def main(args: Array[String]): Unit = {
    CLIArgs.parser.parse(args, CLIArgs.zero.zero).fold(sys.exit(1)) { parsedArgs =>
      ConfigFactory.parseFile(parsedArgs.configFile.toFile).extract[Cfg] match {
        case Success(cfg) =>
          val releaseNotes = get(
            Try(ReleaseNotes(readUtf8(parsedArgs.releaseNotes)))
              .toEither.left.map { t =>
              s"Can't read release notes from ${parsedArgs.releaseNotes}: ${t.asString}"
            }
          )
          val releases = Releases(getS(parsedArgs.releases.map(r =>
            readReleaseDirectory(r resolve cfg.publishInfoJsonFilename)
          ).sequenceValidations))
          val mapping = get(readMapping(cfg.mappingFilePath))

          println(s"Will deploy ${releases.v.size} releases:")
          releases.v.foreach { release =>
            println(s"- $release")
          }
          println()
          println(parsedArgs.updateAppParams)
          println()

          println(s"Using Chrome driver: ${cfg.chromeDriverPath}")
          System.setProperty("webdriver.chrome.driver", cfg.chromeDriverPath.toString)

          io.StdIn.readLine("Press enter to continue...")

          implicit val credentials: Credentials = Credentials(
            username = cfg.credentials.username.getOrElse {
              io.StdIn.readLine("Enter username: ")
            },
            password = cfg.credentials.password.getOrElse {
              print("Enter password: ")
              new String(System.console().readPassword())
            }
          )

          AppUploader(cfg, releaseNotes, releases, mapping, parsedArgs.updateAppParams)
        case Failure(errors) =>
          Console.err.println("Error while reading configs:")
          errors.entries.foreach { error =>
            Console.err.println(s"- ${error.messageWithPath}")
          }
          sys.exit(2)
      }
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

  def readReleaseDirectory(
    publishInfoFile: Path
  ): Either[String, Release] = {
    def publishInfoE =
      Json.parse(Files.readAllBytes(publishInfoFile))
      .validate[PublishInfo]
      .asEither
      .left.map { err => s"Can't parse $publishInfoFile:\n${err.asString}" }
    def apkPathE = {
      val releaseDir = publishInfoFile.getParent
      FileUtils.listFiles(releaseDir.toFile, Array("apk"), false).asScala.toVector match {
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