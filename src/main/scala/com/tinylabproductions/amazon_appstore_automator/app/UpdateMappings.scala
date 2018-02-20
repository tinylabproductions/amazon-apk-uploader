package com.tinylabproductions.amazon_appstore_automator.app

import com.tinylabproductions.amazon_appstore_automator.{AmazonAppId, AmazonAppSku, AndroidPackageName, PackageNameToAppIdMapping}
import com.softwaremill.quicklens._

import scala.annotation.tailrec

trait UpdateMappings { _: App =>
  case class ScrapeAppIds(
    errors: Vector[String],
    warnings: Vector[String],
    mapping: PackageNameToAppIdMapping
  )
  object ScrapeAppIds {
    val empty: ScrapeAppIds = apply(Vector.empty, Vector.empty, PackageNameToAppIdMapping.empty)
  }

  def updateMapping(
    skuMismatchesAreErrors: Boolean,
    known: PackageNameToAppIdMapping
  ): ScrapeAppIds = {
    val AppSkuRe = """^[a-zA-Z_0-9\.]+$""".r
    object AppSkuExtractor {
      def unapply(elem: Element): Option[String] = {
        elem.text.trim match {
          case sku @ AppSkuRe() => Some(sku)
          case _ => None
        }
      }
    }

    type ScrapeIds = (Vector[String], Set[AmazonAppId])
    @tailrec def scrapeIds(
      page: Int, current: ScrapeIds
    ): ScrapeIds = {
      info(s"Checking My Apps page $page...")
      val url = s"https://developer.amazon.com/myapps.html?searchid=&page=$page"
      go to url

      val emptyPage = findOrDie(cssSelector("div.content")).text.contains("Nothing Found")
      if (emptyPage) current
      else {
        val (errors, pageApps) = findAll(cssSelector("#appRow tr td div.title-column a")).map { aElem =>
          aElem.attribute("href").toRight(s"href not found in $aElem!").flatMap {
            case AppLinkRe(appId) => Right(AmazonAppId(appId))
            case _ => Left(s"Can't match URL regexp for $aElem!")
          }
        }.partitionEithers
        val newCurrent =
          current
            .modify(_._1).using(_ ++ errors)
            .modify(_._2).using(_ ++ pageApps)
        scrapeIds(page + 1, newCurrent)
      }
    }

    val (scrapeErrors, scrapedAppIds) = scrapeIds(1, (Vector.empty, Set.empty))
    val unknownIds = scrapedAppIds -- known.mapping.values
    val (detailErrors, mappingsAndWarnings) =
      unknownIds.toVector.zipWithIndex.map { case (appId, idx) =>
        info(s"Retrieving details for $appId ${idx + 1}/${unknownIds.size}...")
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
            if (skuMismatchesAreErrors && !skuMatchesPackage)
              Left(skuMismatchError)
            else {
              val tpl = androidPackageName -> appId
              val warning = if (skuMatchesPackage) None else Some(skuMismatchError)
              Right((tpl, warning))
            }
          }
        } yield result
      }.partitionEithers

    val warnings = mappingsAndWarnings.flatMap(_._2)
    val mappings = mappingsAndWarnings.map(_._1)

    ScrapeAppIds(
      scrapeErrors ++ detailErrors,
      warnings,
      known.modify(_.mapping).using(_ ++ mappings)
    )
  }
}
