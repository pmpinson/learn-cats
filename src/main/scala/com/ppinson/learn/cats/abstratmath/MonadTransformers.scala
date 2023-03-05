package com.ppinson.learn.cats.abstratmath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers extends App {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  import cats.data.EitherT
  import cats.instances.future._
  import cats.syntax.either._

  val bandWiths = Map(
    "server1" -> 50,
    "server2" -> 300,
    "server3" -> 170,
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwith(serverName: String): AsyncResponse[Int] = bandWiths.get(serverName) match {
    case None => EitherT.left(Future("Server not reachable"))
    case Some(v) => EitherT.right(Future(v))
  }

  //  getBandwith("server1").value.foreach(println)
  //  getBandwith("server2").value.foreach(println)

  def canScale(s1: String, s2: String): AsyncResponse[Boolean] = for {
    b1 <- getBandwith(s1)
    b2 <- getBandwith(s2)
  } yield b1 + b2 > 250

  //  canScale("server1", "server2").value.foreach(println)
  //  canScale("server1", "server3").value.foreach(println)

  def genTraffic(s1: String, s2: String): AsyncResponse[String] = for {
    canScale <- canScale(s1, s2)
  } yield if (canScale) s"$s1 and $s2 can scale"
  else s"cannot scale"

  def genTraffic2(s1: String, s2: String): AsyncResponse[String] =
    canScale(s1, s2).transform {
      case Left(m) => m.asLeft
      case Right(false) => s"cannot scale".asLeft
      case Right(true) => s"$s1 and $s2 can scale".asRight
    }

  genTraffic2("server1", "server2").value.foreach(println)
  genTraffic2("server1", "server3").value.foreach(println)
  genTraffic2("server4", "server3").value.foreach(println)
}
