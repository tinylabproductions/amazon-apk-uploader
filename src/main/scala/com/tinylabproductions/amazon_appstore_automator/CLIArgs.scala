package com.tinylabproductions.amazon_appstore_automator

import java.nio.file.{Path, Paths}

import com.tinylabproductions.amazon_appstore_automator.app.UpdateAppParams
import scopt.{OptionParser, Read, Zero}
import com.softwaremill.quicklens._

case class CLIArgs(
  configFile: Path,
  releaseNotes: Path,
  releases: Vector[Path],
  updateAppParams: UpdateAppParams
)
object CLIArgs {
  implicit val zero: Zero[CLIArgs] = Zero.zero(apply(
    null, null, Vector.empty,
    UpdateAppParams(
      submitApp = true, appDirectedUnderAge13 = None
    )
  ))
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
      .action((value, args) => args.modify(_.updateAppParams.submitApp).setTo(value))
    opt[Boolean]("directed-for-kids-under-13").abbr("dfku13")
      .text("if set to yes/no sets the appropriate option in content rating")
      .action((value, args) =>
        args.modify(_.updateAppParams.appDirectedUnderAge13).setTo(Some(value))
      )

    arg[Path]("<directory>...").unbounded()
      .text("A list of directories to deploy")
      .action((value, args) => args.copy(releases = args.releases :+ value))
    note(
      """  These directories should contain publish_info.json (configurable in config file)
        |  and an .apk to upload""".stripMargin
    )

    note(
      """
        |Get Chrome from: https://www.google.com/chrome/
        |Get Chrome driver from: https://sites.google.com/a/chromium.org/chromedriver/
      """.stripMargin)
  }
}