package com.tinylabproductions.amazon_appstore_automator

import java.nio.file.Path

import configs.Configs

sealed trait LogLevel
object LogLevel {
  case object Error extends LogLevel
  case object Warning extends LogLevel
  case object None extends LogLevel

  implicit val configs: Configs[LogLevel] = Configs.stringConfigs.flatMap {
    case "error" => Configs.successful(Error)
    case "warning" => Configs.successful(Warning)
    case "none" => Configs.successful(None)
    case _ => Configs.failure("Unknown value")
  }
}

case class Cfg(
  ignoredAppIds: Set[AmazonAppId],
  amazonAppSkuMustMatchAndroidPackageName: LogLevel,
  publishInfoJsonFilename: String,
  mappingFilePath: Path,
  chromeDriverPath: Path,
  credentials: Cfg.Credentials,
  uploadParalellism: Int
)
object Cfg {
  case class Credentials(username: Option[String], password: Option[String])
}