name := "amazon-appstore-automator"
organization := "com.tinylabproductions"
scalaVersion := "2.12.4"
version := "0.1"

libraryDependencies ++= Seq(
  "org.seleniumhq.selenium" % "selenium-java" % "3.9.1",
  "org.scalatest" %% "scalatest" % "3.0.4",
  "com.github.kxbmap" %% "configs" % "0.4.4",
  "com.softwaremill.quicklens" %% "quicklens" % "1.4.11",
  "com.typesafe.play" %% "play-json" % "2.6.8",
  "commons-io" % "commons-io" % "2.6",
  "org.apache.commons" % "commons-lang3" % "3.7",
  "com.github.scopt" %% "scopt" % "3.7.0"
)

enablePlugins(JavaAppPackaging)
