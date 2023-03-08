package com.ppinson.learn.cats.effects.effects.polymorphic

import cats.effect.syntax.monadCancel._
import cats.effect.{IO, IOApp, MonadCancelThrow}
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.ppinson.learn.cats.effects.effects.{IOExtension, _}

import scala.concurrent.duration._

object PolymorphicCancelleable extends IOApp.Simple {

  val inputPassword: IO[String] =
    IO("Input password:").debug >> IO("Typing password").debug >> IO.sleep(2.seconds) >> IO("RockTheJVM1!")

  def inputPasswordF[F[_]](implicit mc: MonadCancelThrow[F]): F[String] =
    mc.pure("Input password:").debug >> mc.pure("Typing password").debug >> mc.unsafeSleep(2.seconds) >> mc.pure("RockTheJVM1!")

  val verifyPassword: String => IO[Boolean] =
    (pwd: String) => IO("verifying...").debug >> IO.sleep(2.seconds) >> IO(pwd == "RockTheJVM1!")

  def verifyPasswordF[F[_]](pwd: String)(implicit mc: MonadCancelThrow[F]): F[Boolean] =
    mc.pure("verifying...").debug >> mc.unsafeSleep(2.seconds) >> mc.pure(pwd == "RockTheJVM1!")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out. Try Again later").debug.void)
      verified <- verifyPassword(pw)
      _ <- if (verified) IO("Authentication successful.").debug
      else IO("Authentication failed.").debug
    } yield ()
  }

  def authFlowF[F[_]](implicit mc: MonadCancelThrow[F]): F[Unit] = mc.uncancelable { poll =>
    for {
      pw <- poll(inputPasswordF).onCancel(mc.pure("Authentication timed out. Try Again later").debug.void)
      verified <- verifyPasswordF(pw)
      _ <- if (verified) mc.pure("Authentication successful.").debug
      else mc.pure("Authentication failed.").debug
    } yield ()
  }

  def authProgram(timeout: Int): IO[Unit] = for {
    authFib <- authFlow.start
    _ <- IO.sleep(timeout.seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  def authProgramF(timeout: Int): IO[Unit] = for {
    authFib <- authFlowF[IO].start
    _ <- IO.sleep(timeout.seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  override def run: IO[Unit] = authProgramF(1)
}
