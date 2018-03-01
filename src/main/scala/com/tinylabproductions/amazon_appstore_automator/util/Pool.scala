package com.tinylabproductions.amazon_appstore_automator.util


class Pool[A](
  create: () => A,
  destroy: A => Unit
) {
  private[this] var pool = List.empty[A]

  def borrow(): A = synchronized {
    pool match {
      case a :: tail =>
        pool = tail
        a
      case Nil =>
        create()
    }
  }

  def release(a: A): Unit = synchronized {
    pool = a :: pool
  }

  def destroyAll(): Unit = {
    var previousPool = List.empty[A]
    synchronized {
      previousPool = pool
      pool = Nil
    }
    previousPool.foreach(destroy)
  }
}
