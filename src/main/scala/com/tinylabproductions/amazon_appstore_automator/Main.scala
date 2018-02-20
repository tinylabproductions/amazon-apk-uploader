package com.tinylabproductions.amazon_appstore_automator

import java.nio.file.{Path, Paths}

import com.tinylabproductions.amazon_appstore_automator.app.App

object Main {
  def main(args: Array[String]): Unit = {
    run(Paths.get("utils/chromedriver2.exe"))

//    Console.err.println("Usage: amazon-appstore-automator app.conf path_to_chromedriver")
//    Console.err.println()
//    Console.err.println("Get Chrome from: https://www.google.com/chrome/")
//    Console.err.println("Get Chrome driver from: https://sites.google.com/a/chromium.org/chromedriver/")
//    sys.exit(1)
  }

  def run(chromeDriver: Path): Unit = {
    println(s"Using Chrome driver: $chromeDriver")
    System.setProperty("webdriver.chrome.driver", chromeDriver.toString)

    App.work(Cfg(
      mappingFilePath = Paths.get("mapping.json"),
      publishInfoJsonFilename = "publish_info.json"
    ))
  }
}