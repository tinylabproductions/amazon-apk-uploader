package com.tinylabproductions.amazon_appstore_automator

import java.nio.file.Path

import configs.Configs
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath}
import play.api.libs.json.Format._

/* Android package name, for example: com.tinylabproductions.motocross */
case class AndroidPackageName(s: String) extends AnyVal
object AndroidPackageName {
  implicit val format: Format[AndroidPackageName] = jsonFormatStr(apply)(_.s)
}

/**
  * Amazon App Sku, which can differ from android package name to my surprise.
  *
  * For example: com.TinyLabProductions.motocross
  * */
case class AmazonAppSku(s: String) extends AnyVal
object AmazonAppSku {
  implicit val format: Format[AmazonAppSku] = jsonFormatStr(apply)(_.s)
}

/***
  * Application id from amazon developer console.
  *
  * For example in this url:
  * https://developer.amazon.com/application/general/M1BVET9UNPNF8E/detail.html
  *
  * id is M1BVET9UNPNF8E
  **/
case class AmazonAppId(s: String) extends AnyVal
object AmazonAppId {
  implicit val format: Format[AmazonAppId] = jsonFormatStr(apply)(_.s)
  implicit val configs: Configs[AmazonAppId] = Configs.stringConfigs.map(apply)
}

case class PackageNameToAppIdMapping(mapping: Map[AndroidPackageName, AmazonAppId])
object PackageNameToAppIdMapping {
  val empty = apply(Map.empty)

  implicit val format: Format[PackageNameToAppIdMapping] =
    implicitly[Format[Map[String, AmazonAppId]]].inmap(
      m => apply(m.map { case (packageName, appId) => AndroidPackageName(packageName) -> appId }),
      _.mapping.map { case (packageName, appId) => packageName.s -> appId }
    )
}

case class PublishInfo(packageName: AndroidPackageName)
object PublishInfo {
  implicit val format: Format[PublishInfo] =
    (JsPath \ "package_name").format[AndroidPackageName].inmap(apply, _.packageName)
}

case class ReleaseNotes(s: String) extends AnyVal
case class Release(apkPath: Path, publishInfo: PublishInfo) {
  override def toString: String = s"${publishInfo.packageName.s} at $apkPath"
}
case class Releases(v: Vector[Release]) extends AnyVal