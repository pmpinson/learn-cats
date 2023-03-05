package com.ppinson.learn.cats.effects.effects

import cats.effect.{IO, IOApp}

object Traverses extends IOApp.Simple {

  import cats.Traverse
  import cats.syntax.traverse._

  def sequence[A](listOfIos: List[IO[A]]): IO[List[A]] =
    listOfIos.traverse(identity)

//  override def run: IO[Unit] = for {
//    io <- sequence(List(IO(2), IO(4), IO(8)))
//    _ <- IO.println(io)
//  } yield ()

  import cats.instances.option._
  import cats.instances.list._
  def sequence_v2[C[_]: Traverse, A](listOfIos: C[IO[A]]): IO[C[A]] =
//    Traverse[C].traverse(listOfIos)(identity)
  listOfIos.traverse(identity)

  override def run: IO[Unit] = for {
    io <- sequence_v2(List(IO(2), IO(4), IO(8)))
    _ <- IO.println(io)
    io <- sequence_v2(Option(IO(2)))
    _ <- IO.println(io)
  } yield ()
}
