package com.ppinson.learn.cats.effects.effects.coordination

import cats.effect.kernel.Deferred
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Ref}
import cats.syntax.parallel._
import com.ppinson.learn.cats.effects.effects.IOExtension
import com.ppinson.learn.cats.effects.effects.coordination.Mutex.{State, emptyState}

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.util.Random

trait Mutex {

  def acquire: IO[Unit]

  def release: IO[Unit]

  def printState: IO[Unit]

}

class SimpleMutex(state: Ref[IO, State]) extends Mutex {

  def acquire: IO[Unit] = for {
    signal <- Deferred[IO, Unit]
    res <- state.modify {
      case State(false, q) =>
        (State(locked = true, q), IO.unit)
      case State(true, q) =>
        (State(locked = true, q.enqueue(signal)), signal.get)
    }.flatten
  } yield res

  def release: IO[Unit] = state.modify {
    case s@State(false, _) => (s, IO.unit)
    case State(true, q) =>
      if (q.isEmpty) (emptyState, IO.unit)
      else {
        val (signal, rest) = q.dequeue
        (State(locked = true, rest), signal.complete(()).void)
      }
  }.flatten

  def printState: IO[Unit] = for {
    cur <- state.get
    _ <- IO(s"state $cur.").debug
  } yield ()
}

class MutexWithCancelation(state: Ref[IO, State]) extends SimpleMutex(state) {

  def cleanup(signal: Deferred[IO, Unit]): IO[Unit] = state.modify {
    case State(l, q) =>
      (State(l, q.filterNot(_ == signal)), release)
  }

  override def acquire: IO[Unit] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, Unit]
      res <- state.modify {
        case State(false, q) =>
          (State(locked = true, q), IO.unit)
        case State(true, q) =>
          (State(locked = true, q.enqueue(signal)), poll(signal.get).onCancel(cleanup(signal)))
      }.flatten
    } yield res
  }
}

object Mutex {

  case class State(locked: Boolean, queue: Queue[Deferred[IO, Unit]])

  val emptyState: State = State(locked = false, Queue.empty)

  def createSimple: IO[Mutex] = Ref[IO].of(emptyState).map(new SimpleMutex(_))

  def createWithCancelation: IO[Mutex] = Ref[IO].of(emptyState).map(new MutexWithCancelation(_))
}

object MutexPlayground extends IOApp.Simple {

  def criticalTask(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def createNonLockingTask(id: Int): IO[Int] = for {
    _ <- IO(s"[task $id] working...").debug
    res <- criticalTask()
    _ <- IO(s"[task $id] got result $res").debug
  } yield res

  def demoNonLockingTasks(): IO[List[Int]] = (1 to 10).toList.parTraverse(createNonLockingTask)

  def createLockingTask(id: Int, mutex: Mutex): IO[Int] = (for {
    _ <- IO(s"[task $id] waiting for permission...").debug
    _ <- mutex.acquire
    _ <- IO(s"[task $id] working...").debug
    res <- criticalTask()
    _ <- IO(s"[task $id] got result $res").debug
    _ <- mutex.release
    _ <- IO(s"[task $id] lock remove").debug
  } yield res).onCancel(IO(s"[task $id] was canceled").debug.void)

  def createCancelingTask(id: Int, mutex: Mutex): IO[Int] =
    if (id % 4 != 0) createLockingTask(id, mutex)
    else for {
      fib <- createLockingTask(id, mutex).start
      _ <- IO.sleep(2.second) >> fib.cancel
      res <- fib.join
      out <- res match {
        case Succeeded(fa) => fa
        case Errored(_) => IO(-1)
        case Canceled() => IO(-2)
      }
    } yield out

  def demoLockingTasks(): IO[List[Int]] = for {
    mutex <- Mutex.createSimple
    res <- (1 to 10).toList.parTraverse(createLockingTask(_, mutex))
  } yield res

  def demoLockingTasksWithCancelation(): IO[List[Int]] = for {
    mutex <- Mutex.createWithCancelation
    res <- (1 to 10).toList.parTraverse(createCancelingTask(_, mutex))
    _ <- mutex.printState
  } yield res

  //  override def run: IO[Unit] = demoLockingTasksWithCancelation().debug.void


  def demoCancelWhileBlocked(): IO[Unit] = for {
    mutex <- Mutex.createWithCancelation
    fib1 <- (
      IO("[fib1] getting the mutex").debug >>
        mutex.acquire >>
        IO("[fib1] get the mutex and never release !").debug >>
        IO.never
      ).start
    fib2 <- (
      IO("[fib2] sleeping").debug >>
        IO.sleep(1.second) >>
        IO("[fib2] trying to acquire the mutex").debug >>
        mutex.acquire >>
        IO("[fib2] acquire the mutex").debug >>
        IO.never
      ).start
    fib3 <- (
      IO("[fib3] sleeping").debug >>
        IO.sleep(1500.millis) >>
        IO("[fib3] trying to acquire the mutex").debug >>
        mutex.acquire >>
        IO("[fib3] acquire the mutex").debug >>
        IO.never
      ).start
    _ <- IO.sleep(2.seconds) >> IO("CANCELLING fib2 !").debug >> fib2.cancel
    _ <- fib1.join
    _ <- fib2.join
    _ <- fib3.join
  } yield ()


  override def run: IO[Unit] = demoCancelWhileBlocked()
//  override def run: IO[Unit] = (for {
//    fib <- demoLockingTasksWithCancelation().start
//    _ <- IO.sleep(3.second) >> fib.cancel
//    res <- fib.join
//  } yield res).debug.void
}
