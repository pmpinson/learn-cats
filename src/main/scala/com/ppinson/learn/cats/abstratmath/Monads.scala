package com.ppinson.learn.cats.abstratmath


object Monads extends App {

  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val numberList = List(1, 2, 3)
  val charList = List('a', 'b', 'c')

  // 1.1
  println(numberList.flatMap(n => charList.map((n, _))))
  println((for {
    n <- numberList
    c <- charList
  } yield (n, c)))

  val numberOption = Option(2)
  val numberNone: Option[Int] = None
  val charOption = Option('c')
  val charNone: Option[Char] = None
  println((for {
    n <- numberOption
    c <- charOption
  } yield (n, c)))
  println((for {
    n <- numberNone
    c <- charOption
  } yield (n, c)))
  println((for {
    n <- numberOption
    c <- charNone
  } yield (n, c)))

  def getPairs[F[_] : Monad](ma: F[Int], mb: F[Char]): F[(Int, Char)] =
    ma.flatMap(a => mb.map((a, _)))

  println(getPairs(numberList, charList))
  println(getPairs(numberOption, charOption))

  trait MyMonad[M[_]] {
    def pure[A](v: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  def getPairs2[F[_] : Monad, A, B](ma: F[A], mb: F[B]): F[(A, B)] = for {
    a <- ma
    b <- mb
  } yield (a, b)

  println(getPairs2(numberList, charList))
  println(getPairs2(numberOption, charOption))
}
