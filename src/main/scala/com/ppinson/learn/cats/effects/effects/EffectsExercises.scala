package com.ppinson.learn.cats.effects.effects

import scala.io.StdIn

object EffectsExercises extends App {

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      new MyIO[B](() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      new MyIO[B](() => f(unsafeRun()).unsafeRun())
  }

  def currentTimeIO(): MyIO[Long] = new MyIO[Long](() => System.currentTimeMillis())

  def measureIO[A](computation: MyIO[A]): MyIO[Long] = for {
    start <- currentTimeIO()
    _ <- computation
    end <- currentTimeIO()
  } yield end - start

  def printlnIO(msg: String): MyIO[Unit] = new MyIO[Unit](() => println(msg))

  def scanIO(): MyIO[String] = new MyIO[String](() => {println("Express yourself:");StdIn.readLine()})

  currentTimeIO().unsafeRun()
  printlnIO("Hello").unsafeRun()
  printlnIO("What did you say: " + scanIO().unsafeRun()).unsafeRun()

  println("It took: " + measureIO(scanIO()).unsafeRun() + " millis")
}
