package com.ppinson.learn.cats.effects.effects

import cats.effect.IO

import scala.annotation.tailrec

object IOEffectsExercises extends App {


  def sequenceTakeLast[A, B](ioA: IO[A], ioB: IO[B]): IO[B] =
  //    ioA.flatMap(_ => ioB)
    for {
      _ <- ioA
      b <- ioB
    } yield b

  def sequenceTakeFirst[A, B](ioA: IO[A], ioB: IO[B]): IO[A] =
  //    ioA.flatMap(a => ioB.map(_ => a))
    for {
      a <- ioA
      _ <- ioB
    } yield a

  def forever[A](anIo: IO[A]): IO[A] = {
    anIo.flatMap(_ => forever(anIo))
  }

  def convert[A, B](anIO: IO[A], value: B): IO[B] = anIO.map(_ => value)

  def asUnit[A](anIO: IO[A]): IO[Unit] = convert(anIO, ())

  //  @tailrec
  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO.delay(0)
    else for {
      cur <- IO.delay(n)
      next <- sumIO(n - 1)
    } yield next + cur

  def fiboNotTail(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fiboNotTail(n - 2) + fiboNotTail(n - 1)
  }

  def fibo(n: Int): Int = {
    @tailrec
    def fibFcn(n: Int, acc1: Int, acc2: Int): Int = n match {
      case 0 => acc1
      case 1 => acc2
      case _ => fibFcn(n - 1, acc2, acc1 + acc2)
    }

    fibFcn(n, 0, 1)
  }

  def fiboIONotTail(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else if (n == 1) IO(1)
    else for {
      nm1 <- fiboIONotTail(n - 1)
      nm2 <- fiboIONotTail(n - 2)
    } yield nm1 + nm2

  def fiboIO(n: Int): IO[Int] = {
    @tailrec
    def fibFcn(n: Int, acc1: IO[Int], acc2: IO[Int]): IO[Int] = n match {
      case 0 => acc1
      case 1 => acc2
      case _ => fibFcn(n - 1, acc2, acc1.flatMap(ac1 => acc2.map(ac2 => ac1 + ac2)))
    }

    fibFcn(n, IO.pure(0), IO.pure(1))
  }

  import cats.effect.unsafe.implicits.global

  println(sequenceTakeFirst(IO.delay(12), IO.delay(15)).unsafeRunSync())
  println(sequenceTakeLast(IO.delay(12), IO.delay(15)).unsafeRunSync())
  //  println(forever(IO.delay(println("running !"))).unsafeRunSync())
  println(convert(IO.delay(12), "'148'").unsafeRunSync())
  println(asUnit(IO.delay(12)).unsafeRunSync())
  println(sumIO(20000).unsafeRunSync())

  println(fiboIO(0).unsafeRunSync())
  println(fiboIO(1).unsafeRunSync())
  println(fiboIO(2).unsafeRunSync())
  println(fiboIO(3).unsafeRunSync())
  println(fiboIO(4).unsafeRunSync())
  println(fiboIO(5).unsafeRunSync())
  println(fiboIO(6).unsafeRunSync())
  println(fiboIO(7).unsafeRunSync())
  println(fiboIO(15).unsafeRunSync())
  println(fiboIO(20).unsafeRunSync())
  println(fiboIO(30).unsafeRunSync())
  println(fiboIO(40).unsafeRunSync())
  println(fiboIONotTail(30).unsafeRunSync())

  //  println(fibo(0))
  //  println(fibo(1))
  //  println(fibo(2))
  //  println(fibo(3))
  //  println(fibo(4))
  //  println(fibo(5))
  //  println(fibo(6))
  //  println(fibo(7))
  //  println(fibo(15))
  //  println(fibo(20))
  //  println(fibo(30))
  //  println(fibo(40))
}
