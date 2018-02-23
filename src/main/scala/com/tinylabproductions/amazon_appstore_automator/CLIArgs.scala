package com.tinylabproductions.amazon_appstore_automator

import java.nio.file.{Path, Paths}

import scopt.{OptionParser, Read, Zero}

case class CLIArgs(
  configFile: Path,
  releaseNotes: Path,
  releasesPath: Path,
  submitApp: Boolean = true
)
object CLIArgs {
  implicit val zero: Zero[CLIArgs] = Zero.zero(apply(null, null, null))
  implicit val PathRead: Read[Path] = Read.stringRead.map(Paths.get(_))

  val parser: OptionParser[CLIArgs] = new scopt.OptionParser[CLIArgs]("amazon-appstore-automator") {
    opt[Path]('c', "config").required()
      .text("Path to app.conf")
      .action((value, args) => args.copy(configFile = value))
    opt[Path]("release-notes").abbr("rn").required()
      .text("file with release notes that will be uploaded to app store")
      .action((value, args) => args.copy(releaseNotes = value))
    opt[Boolean]('s', "submit-app")
      .text("if set to no, uploads binary and release notes but does not actually submit the app")
      .action((value, args) => args.copy(submitApp = value))
    opt[Path]('r', "releases-path").abbr("rp").required()
      .text("directory to deploy")
      .action((value, args) => args.copy(releasesPath = value))
    note(
      """  This directory should have subdirectories that each contains
        |  publish_info.json (configurable in config file) and an .apk to upload""".stripMargin
    )

    note(
      """
        |Get Chrome from: https://www.google.com/chrome/
        |Get Chrome driver from: https://sites.google.com/a/chromium.org/chromedriver/
      """.stripMargin)
  }
}