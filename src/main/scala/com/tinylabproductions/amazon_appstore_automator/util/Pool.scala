package com.tinylabproductions.amazon_appstore_automator.util

import scala.util.control.NonFatal


class Pool[A](
  create: () => A,
  destroy: A => Unit
) {
  private[this] var pool = List.empty[A]

  def borrow(): A = {
    val aOpt = synchronized {
      pool match {
        case a :: tail =>
          pool = tail
          Some(a)
        case Nil =>
          None
      }
    }
    aOpt.getOrElse(create())
  }

  def release(a: A): Unit = synchronized {
    pool = a :: pool
  }

  def withSession[B](f: A => B): B = {
    val a = borrow()
    try {
      val ret = f(a)
      release(a)
      ret
    }
    catch {
      case NonFatal(t) =>
        destroy(a)
        throw t
    }
  }

  def destroyN(n: Int): Unit = {
    if (n <= 0) return

    var previousPool = List.empty[A]
    synchronized {
      val (toDestroy, toKeep) = pool.splitAt(n)
      previousPool = toDestroy
      pool = toKeep
    }
    previousPool.foreach(destroy)
  }

  def destroyAll(): Unit = destroyN(pool.size)
}
