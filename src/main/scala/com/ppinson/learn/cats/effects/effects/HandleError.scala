package com.ppinson.learn.cats.effects.effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object HandleError extends App {

  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match {
    case Some(v) => IO(v)
    case None => IO.raiseError(ifEmpty)
  }

  def try2IO[A](t: Try[A]): IO[A] = t match {
    case Success(v) => IO(v)
    case Failure(t) => IO.raiseError(t)
  }

  def either2IO[A](e: Either[Throwable, A]): IO[A] = e match {
    case Right(v) => IO(v)
    case Left(t) => IO.raiseError(t)
  }

  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A]=
    io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A]=
    io.redeemWith(handler, IO(_))

}
