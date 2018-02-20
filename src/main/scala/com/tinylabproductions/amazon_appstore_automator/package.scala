package com.tinylabproductions

import play.api.libs.functional.syntax._
import play.api.libs.json.Format

package object amazon_appstore_automator {
  def jsonFormatStr[A](f: String => A)(g: A => String): Format[A] =
    implicitly[Format[String]].inmap(f, g)

  implicit class TraversableOnceExts[A](val to: TraversableOnce[A]) extends AnyVal {
    def partitionEithers[L, R](implicit ev: A <:< Either[L, R]): (Vector[L], Vector[R]) = {
      val lefts = Vector.newBuilder[L]
      val rights = Vector.newBuilder[R]
      to.foreach { a =>
        ev(a) match {
          case Left(l) => lefts += l
          case Right(r) => rights += r
        }
      }
      (lefts.result(), rights.result())
    }

    def sequenceValidations[L, R](implicit ev: A <:< Either[L, R]): Either[Vector[L], Vector[R]] = {
      val errors = Vector.newBuilder[L]
      val results = Vector.newBuilder[R]
      var hasErrors = false
      to.foreach { a =>
        ev(a) match {
          case Left(err) =>
            errors += err
            hasErrors = true
          case Right(result) if !hasErrors =>
            results += result
          case Right(_) =>
        }
      }
      if (hasErrors) Left(errors.result())
      else Right(results.result())
    }
  }
}
