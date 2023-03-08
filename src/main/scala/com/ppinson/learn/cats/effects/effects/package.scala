package com.ppinson.learn.cats.effects

import cats.{Applicative, Functor}
import cats.effect.{IO, MonadCancel}
import cats.syntax.functor._

import scala.concurrent.duration.FiniteDuration

package object effects {
  implicit class IOExtension[A](io: IO[A]) {
    def debug: IO[A] = for {
      a <- io
      t = Thread.currentThread().getName
      _ = println(s"[$t] $a")
    } yield a
  }

  implicit class FunctorExtension[F[_], A](fa: F[A]) {
    def debug(implicit f: Functor[F]): F[A] = fa.map(a => {
      val t = Thread.currentThread().getName
      println(s"[$t] $a")
      a
    })

    def unsafeSleep[F[_]](duration: FiniteDuration)(implicit mc: Applicative[F]): F[Unit] =
      mc.pure(Thread.sleep(duration.toMillis))
  }
}
