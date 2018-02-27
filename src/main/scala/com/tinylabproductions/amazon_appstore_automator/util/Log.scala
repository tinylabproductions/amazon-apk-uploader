package com.tinylabproductions.amazon_appstore_automator.util

import java.time.LocalDateTime

trait Log {
  def err(o: Any): Unit = Console.err.println(s"[${LocalDateTime.now()}] !!! $o")
  def warn(o: Any): Unit = println(s"[${LocalDateTime.now()}] *** $o")
  def info(o: Any): Unit = println(s"[${LocalDateTime.now()}] $o")
}
object Log extends Log
