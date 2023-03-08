package com.ppinson.learn.cats.effects.effects.polymorphic

import cats.effect.kernel.Temporal
import cats.effect.syntax.spawn._
import cats.effect.syntax.temporal._
import cats.effect.{IO, IOApp}
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import com.ppinson.learn.cats.effects.effects.IOExtension

import scala.concurrent.duration._

object PolymorphicTemporalSuspension extends IOApp.Simple {

  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(implicit tempo: Temporal[F]): F[A] =
    fa.race(tempo.sleep(duration)).flatMap {
      case Left(v) => v.pure
      case Right(_) => new RuntimeException("Computation timed out.").raiseError
    }

  def timeout2[F[_], A](fa: F[A], duration: FiniteDuration)(implicit tempo: Temporal[F]): F[A] =
    fa.timeout(duration)

  val aComputation: IO[String] = IO("starting").debug >> IO.sleep(2.seconds) >> IO("finished").debug

  override def run: IO[Unit] = timeout2(aComputation, 3.seconds).void
}
