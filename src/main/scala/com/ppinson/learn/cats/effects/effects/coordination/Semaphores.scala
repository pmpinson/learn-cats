package com.ppinson.learn.cats.effects.effects.coordination

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import cats.syntax.parallel._
import com.ppinson.learn.cats.effects.effects.IOExtension

import scala.concurrent.duration._
import scala.util.Random

object Semaphores extends IOApp.Simple {

  def doWorkWhileLoggedIn(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  val mutex: IO[Semaphore[IO]] = Semaphore[IO](1)

  def users(sem: Semaphore[IO]): IO[List[Int]] = (1 to 10).toList.parTraverse { id =>
    for {
      _ <- IO(s"[session $id] waiting to log in...").debug
      _ <- sem.acquire
      _ <- IO(s"[session $id] logged in, working...").debug
      res <- doWorkWhileLoggedIn()
      _ <- IO(s"[session $id] done: $res, logging out...").debug
      _ <- sem.release
    } yield res
  }

  def demoMutex(): IO[List[Int]] = for {
    sem <- mutex
    res <- users(sem)
  } yield res

  override def run: IO[Unit] = demoMutex().debug.void
}
