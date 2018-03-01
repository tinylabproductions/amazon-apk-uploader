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

  def withSession[B](f: A => B): B = {
    val a = borrow()
    try { f(a) }
    finally { release(a) }
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
