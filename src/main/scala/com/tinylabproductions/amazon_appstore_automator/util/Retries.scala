package com.tinylabproductions.amazon_appstore_automator.util

import scala.util.control.NonFatal

object Retries {
  def withRetries[A](name: String, retries: Int)(f: => A): A = {
    def doTry(tryNo: Int): A = {
      try { f }
      catch {
        case NonFatal(t) if tryNo <= retries =>
          Console.err.println(s"$name failed try $tryNo with $t, retrying")
          doTry(tryNo + 1)
      }
    }

    doTry(1)
  }
}
