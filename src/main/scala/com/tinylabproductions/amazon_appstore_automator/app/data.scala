package com.tinylabproductions.amazon_appstore_automator.app

import cats.kernel.Monoid
import com.tinylabproductions.amazon_appstore_automator.{AmazonAppId, AndroidPackageName, HasErrors, HasWarnings, PackageNameToAppIdMapping}

case class Credentials(username: String, password: String)

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
