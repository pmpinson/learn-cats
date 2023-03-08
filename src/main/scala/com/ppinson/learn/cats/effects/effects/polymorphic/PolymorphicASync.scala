package com.ppinson.learn.cats.effects.effects.polymorphic

import cats.effect.{Concurrent, Sync}
import cats.effect.{IO, IOApp, MonadCancel}

import scala.io.StdIn

import cats.effect.syntax.monadCancel._
import cats.effect.{IO, IOApp, MonadCancelThrow}
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.ppinson.learn.cats.effects.effects.{IOExtension, _}

import scala.concurrent.duration._

object PolymorphicASync extends IOApp.Simple {

  def firstEffect[F[_]: Concurrent, A](a: A): F[A] = Concurrent[F].pure(a)
  def secondEffect[F[_]: Sync, A](a: A): F[A] = Sync[F].pure(a)

  def tupledEffect[F[_], A](a: A)(implicit C: Concurrent[F], S: Sync[F]): F[(A, A)] = {
    C.flatMap(firstEffect(a))(a1 => S.map(secondEffect(a))(a2 => {
      (a1, a2)
    }))
  }

  override def run: IO[Unit] = tupledEffect[IO, Int](120).debug.void
}
