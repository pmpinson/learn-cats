package com.ppinson.learn.cats.effects.effects.coordination

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.{FiberIO, IO, IOApp, OutcomeIO}
import cats.syntax.parallel._
import com.ppinson.learn.cats.effects.effects.IOExtension

import scala.concurrent.duration._

object DeferredExercices extends IOApp.Simple {

  def timesUp(): IO[Unit] = {

    def incrementer(counter: Ref[IO, Long], notifier: Deferred[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      next <- counter.getAndUpdate(_ + 1)
      _ <- IO(s"[incrementer] increment $next").debug
      _ <- if (next >= 10) notifier.complete(next) else incrementer(counter, notifier)
    } yield ()

    def notifier(defer: Deferred[IO, Long]): IO[Unit] = for {
      next <- defer.get
      _ <- IO(s"[notifier] Time's Up for $next").debug
    } yield ()

    for {
      counter <- Ref[IO].of(0L)
      defer <- Deferred[IO, Long]
      _ <- (incrementer(counter, defer), notifier(defer)).parTupled
    } yield ()
  }

  /**
   * Exercises:
   *  - (medium) write a small alarm notification with two simultaneous IOs
   *    - one that increments a counter every second (a clock)
   *    - one that waits for the counter to become 10, then prints a message "time's up!"
   *
   *  - (mega hard) implement racePair with Deferred.
   *    - use a Deferred which can hold an Either[outcome for ioa, outcome for iob]
   *    - start two fibers, one for each IO
   *    - on completion (with any status), each IO needs to complete that Deferred
   *      (hint: use a finalizer from the Resources lesson)
   *      (hint2: use a guarantee call to make sure the fibers complete the Deferred)
   *    - what do you do in case of cancellation (the hardest part)?
   */

  type RacePairResult[A, B] = Either[
    (OutcomeIO[A], FiberIO[B]), // (winner result, loser fiber)
    (FiberIO[A], OutcomeIO[B]) // (loser fiber, winner result)
  ]

  def ourRacePair[A, B](ioA: IO[A], ioB: IO[B]): IO[RacePairResult[A, B]] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, Either[OutcomeIO[A], OutcomeIO[B]]]
      fibA <- ioA.guaranteeCase(r => signal.complete(Left(r)).void).start
      fibB <- ioB.guaranteeCase(r => signal.complete(Right(r)).void).start
      signalRes <- poll(signal.get <* (fibA.cancel, fibB.cancel).parTupled).onCancel((fibA.cancel, fibB.cancel).parTupled.void)
    } yield signalRes match {
      case Left(resA) => Left((resA, fibB))
      case Right(resB) => Right((fibA, resB))
    }
  }

  def process[A](name: String, out: A, processTime: Int): IO[A] = (for {
    _ <- IO(s"[fib $name] START").debug
    _ <- IO.sleep(processTime.second)
  } yield out)
    .guaranteeCase {
      case Succeeded(_) => IO(s"[fib $name] success").debug.void
      case Errored(_) => IO(s"[fib $name] in error").debug.void
      case Canceled() => IO(s"[fib $name] canceled").debug.void
    }

  val myRace: IO[RacePairResult[Int, String]] = ourRacePair(
    process("A", 42, 3),
    process("B", "the meaning of life", 5)
  )

  // run
//  override def run: IO[Unit] = myRace.debug.void

  // run and cancel
  override def run: IO[Unit] = (for {
    res <- myRace.start
    _ <- IO.sleep(2.second) >> res.cancel
    _ <- res.join
  } yield res).debug.void

}
