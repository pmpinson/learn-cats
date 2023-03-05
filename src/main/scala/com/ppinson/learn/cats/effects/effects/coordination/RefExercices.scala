package com.ppinson.learn.cats.effects.effects.coordination

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.parallel._
import com.ppinson.learn.cats.effects.effects.IOExtension

import scala.concurrent.duration._

object RefExercices extends IOApp.Simple {

  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L

    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- IO(ticks += 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.second)
      _ <- IO(s"TICKS: $ticks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def tickingClock(): IO[Unit] = {
    def tickingClock(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ticks.update(v => v + 1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(5.second)
      current <- ticks.get
      _ <- IO(s"TICKS: $current").debug
      _ <- printTicks(ticks)
    } yield ()

    for {
      ticks <- Ref[IO].of(0L)
      _ <- (tickingClock(ticks), printTicks(ticks)).parTupled
    } yield ()
  }

  override def run: IO[Unit] = tickingClock()
}
