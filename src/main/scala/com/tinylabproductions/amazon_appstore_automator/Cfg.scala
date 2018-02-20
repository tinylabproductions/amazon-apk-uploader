package com.tinylabproductions.amazon_appstore_automator

import java.nio.file.Path

case class Cfg(
  amazonAppSkuMustMatchAndroidPackageName: Boolean,
  publishInfoJsonFilename: String,
  mappingFilePath: Path
)
