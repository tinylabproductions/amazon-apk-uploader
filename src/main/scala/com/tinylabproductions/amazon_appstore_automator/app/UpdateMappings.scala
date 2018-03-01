package com.tinylabproductions.amazon_appstore_automator.app

import cats.instances.all._
import cats.kernel.Monoid
import cats.syntax.all._
import com.tinylabproductions.amazon_appstore_automator._
import com.tinylabproductions.amazon_appstore_automator.util.Log._

import scala.util.matching.Regex

case class FetchAppIdsResult(errors: Vector[String], ids: Set[AmazonAppId]) extends HasErrors {
  def isEmpty: Boolean = ids.isEmpty
}
object FetchAppIdsResult {
  val empty = apply(Vector.empty, Set.empty)

  implicit object monoid extends Monoid[FetchAppIdsResult] {
    def empty = FetchAppIdsResult.empty

    def combine(a1: FetchAppIdsResult, a2: FetchAppIdsResult) =
      FetchAppIdsResult(a1.errors ++ a2.errors, a1.ids ++ a2.ids)
  }
}

case class ScrapeAppId(warning: Option[String], pkgName: AndroidPackageName, appId: AmazonAppId) {
  def toIds = ScrapeAppIds(
    Vector.empty, warning.toVector, PackageNameToAppIdMapping(Map(pkgName -> appId))
  )
}
case class ScrapeAppIds(
  errors: Vector[String],
  warnings: Vector[String],
  map: PackageNameToAppIdMapping
) extends HasErrors with HasWarnings {
  def ++(ids: ScrapeAppIds): ScrapeAppIds =
    ScrapeAppIds(errors ++ ids.errors, warnings ++ ids.warnings, map ++ ids.map)
}
object ScrapeAppIds {
  val empty: ScrapeAppIds = apply(Vector.empty, Vector.empty, PackageNameToAppIdMapping.empty)
}

trait UpdateMappings { _: App =>
  def scrapeIds(page: Int): FetchAppIdsResult = {
    info(s"Checking My Apps page $page...")
    go to s"https://developer.amazon.com/myapps.html?searchid=&page=$page"

    val emptyPage = findOrDie(cssSelector("div.content")).text.contains("Nothing Found")
    if (emptyPage) FetchAppIdsResult.empty
    else {
      val (errors, pageApps) = findAll(cssSelector("#appRow tr td div.title-column a")).map { aElem =>
        aElem.attribute("href").toRight(s"href not found in $aElem!").flatMap {
          case AppLinkRe(appId) => Right(AmazonAppId(appId))
          case _ => Left(s"Can't match URL regexp for $aElem!")
        }
      }.partitionEithers
      FetchAppIdsResult(errors, pageApps.toSet)
    }
  }

  val AppSkuRe: Regex = """^[a-zA-Z_0-9\.]+$""".r
  object AppSkuExtractor {
    def unapply(elem: Element): Option[String] = {
      elem.text.trim match {
        case sku @ AppSkuRe() => Some(sku)
        case _ => None
      }
    }
  }

  def scrapeAppId(onSkuMismatches: LogLevel, appId: AmazonAppId): Either[String, ScrapeAppId] = {
    go to appUrl(appId)

    // The element seems to be loaded asynchronously, therefore we try go get it multiple times.
    def getAmazonAppSku(retry: Int): Either[String, AmazonAppSku] = {
      val appSkuElem =
        find(cssSelector("#app-root > div > div.appDetails > div:nth-child(2) > div > div"))

      val result =
        appSkuElem
          // We might have multiple hits, so try to find the real one.
          .collectFirst { case AppSkuExtractor(sku) => AmazonAppSku(sku) }
          .toRight(s"Can't extract App SKU for $appId from ${appSkuElem.map(asString)}")

      if (result.isLeft) {
        if (retry < 20) {
          val timeout = 50 * (retry + 1)
          info(s"Retrying app sku retrieval for $appId in ${timeout}ms")
          Thread.sleep(timeout)
          getAmazonAppSku(retry + 1)
        }
        else {
          err(s"Failed to retrieve app sku for $appId.")
          result
        }
      }
      else {
        result
      }
    }

    def fetchAndroidPackageName(
      amazonAppSku: AmazonAppSku
    ): Either[String, AndroidPackageName] = {
      goToBinaryFiles()
      find(id("manifest_ro_PACKAGE"))
        .map(elem => AndroidPackageName(elem.text.trim))
        .toRight(s"Can't find android package name for $amazonAppSku, $appId")
    }

    for {
      amazonAppSku <- getAmazonAppSku(0)
      androidPackageName <- fetchAndroidPackageName(amazonAppSku)
      skuMatchesPackage = amazonAppSku.s == androidPackageName.s
      result <- {
        def skuMismatchError = s"$amazonAppSku != $androidPackageName for $appId"

        if (onSkuMismatches == LogLevel.Error && !skuMatchesPackage)
          Left(skuMismatchError)
        else {
          val warning =
            if (!skuMatchesPackage && onSkuMismatches == LogLevel.Warning) Some(skuMismatchError)
            else None
          Right(ScrapeAppId(warning, androidPackageName, appId))
        }
      }
    } yield result
  }
}
