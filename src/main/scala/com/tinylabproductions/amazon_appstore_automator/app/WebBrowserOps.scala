package com.tinylabproductions.amazon_appstore_automator.app

import org.openqa.selenium.WebDriver
import org.scalatest.selenium.WebBrowser

trait WebBrowserOps { _: WebBrowser =>
  def findOrDie(query: Query, name: Option[String] = None)(implicit driver: WebDriver): Element =
    find(query).getOrElse(sys.error(
      s"Can't find element${name.fold("")(s => s" '$s'")} by $query, aborting!"
    ))

  def findOrDie(query: Query, name: String)(implicit driver: WebDriver): Element =
    findOrDie(query, Some(name))

  def asString(e: Element): String = s"Element[text: '${e.text}', $e]"
}
