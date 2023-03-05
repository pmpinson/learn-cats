package com.ppinson.learn.cats.effects.effects

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object FiberExercices extends IOApp.Simple {

  def processFiberOut[A](result: Outcome[IO, Throwable, A], cancelMsg: String = "IO was Canceled") = result match {
    case Succeeded(fa) => fa
    case Errored(e) => IO.raiseError(e)
    case Canceled() => IO.raiseError(new RuntimeException(cancelMsg))
  }

  def processResultsFromFiber[A](io: IO[A]): IO[A] = for {
    fiber <- io.start
    result <- fiber.join
    realResult <- processFiberOut(result)
  } yield realResult

  def tupledIOs[A, B](ioA: IO[A], ioB: IO[B]): IO[(A, B)] = for {
    fiberA <- ioA.start
    fiberB <- ioB.start
    resultA <- fiberA.join
    resultB <- fiberB.join
    realResult <- (resultA, resultB) match {
      case (Succeeded(fa), Succeeded(fb)) => fa.flatMap(a => fb.map(b => (a, b)))
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case (Canceled(), _) => IO.raiseError(new RuntimeException("IO 1 was Canceled"))
      case (_, Canceled()) => IO.raiseError(new RuntimeException("IO 2 was Canceled"))
    }
  } yield realResult

    def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = for {
      fiber <- io.debug.start
      _ <- (IO.sleep(duration) >> fiber.cancel).start
//      _ <- IO.sleep(duration) >> fiber.cancel
      result <- fiber.join
      realResult <- processFiberOut(result, "IO was timeout")
    } yield realResult

  def computation(label: String, res: Int, sleep: FiniteDuration = 1.second): IO[Int] =
    IO(s"starting $label").debug >> IO.sleep(sleep) >> IO(s"ending $label").debug >> IO(res)

  def computationCancel(label: String, res: Int, sleep: FiniteDuration = 1.second): IO[Int] = for {
    fib <- computation(label, res, sleep).start
    _ <- IO.sleep(500.millis) >> fib.cancel
    result <- fib.join
    realResult <- processFiberOut(result)
  } yield realResult

  // processResultsFromFiber normal
  //  override def run: IO[Unit] = processResultsFromFiber(computation("fib 1", 500)).debug.void
  // processResultsFromFiber cancelled
//    override def run: IO[Unit] = processResultsFromFiber(computationCancel("fib 1", 500)).debug.void

//   tupledIOs normal
//      override def run: IO[Unit] = tupledIOs(computation("fib 1", 500), computation("fib 2", 1000, 2.second)).debug.void
//   tupledIOs second canceled
//      override def run: IO[Unit] =tupledIOs(computation("fib 1", 500), computationCancel("fib 2", 1000, 2.second)).debug.void 2.second)).debug.void

  // timeout not raise
  override def run: IO[Unit] = timeout(computation("fib 1", 500, 500.millis), 2.second).debug.void
  // timeout raised
//  override def run: IO[Unit] = timeout(computation("fib 1", 500, 2.second), 1.second).debug.void
}

