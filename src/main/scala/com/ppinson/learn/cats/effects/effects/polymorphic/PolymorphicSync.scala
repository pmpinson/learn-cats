package com.ppinson.learn.cats.effects.effects.polymorphic

import cats.effect.kernel.{Sync, Temporal}
import cats.effect.syntax.spawn._
import cats.effect.syntax.temporal._
import cats.effect.{IO, IOApp, MonadCancel}
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.ppinson.learn.cats.effects.effects.IOExtension

import java.util.Scanner
import scala.concurrent.duration._
import scala.io.StdIn

object PolymorphicSync extends IOApp.Simple {

  trait MySync[F[_]] extends MonadCancel[F, Throwable] {
    def delay[A](thunk: => A): F[A] = ???
    def blocking[A](thunk: => A): F[A] = ???

    def defer[A](thunk: => F[A]): F[A] =
      flatMap(delay(thunk))(identity)
  }

  trait Console[F[_]] {
    def println[A](a: A): F[Unit]
    def readLine(): F[String]
  }

  object Console {
    def apply[F[_]](implicit sync: Sync[F]): F[Console[F]] = sync.pure(new Console[F] {
      override def println[A](a: A): F[Unit] = sync.delay(System.out.println(a))

      override def readLine(): F[String] = sync.delay(StdIn.readLine())
    })
  }

  override def run: IO[Unit] = for {
    console <- Console[IO]
    _ <- console.println("hello")
    _ <- console.println("input something")
    in <- console.readLine()
    _ <- console.println(s"you said: $in")
  } yield ()
}
