package com.tinylabproductions.amazon_appstore_automator.util

import java.time.LocalDateTime

trait Log {
  def err(o: Any): Unit = Console.err.println(s"${header()} !!! $o")
  def warn(o: Any): Unit = println(s"${header()} *** $o")
  def info(o: Any): Unit = println(s"${header()} $o")

  private[this] def header(): String = s"[${LocalDateTime.now()}|T${Thread.currentThread().getId}]"
}
object Log extends Log
