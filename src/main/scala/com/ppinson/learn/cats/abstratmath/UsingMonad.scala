package com.ppinson.learn.cats.abstratmath

import scala.util.{Failure, Success, Try}

object UsingMonad extends App {

  import cats.syntax.either._

  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  val tryHttpServiceInstance = new HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] = (cfg.get("host"), cfg.get("port")) match {
      case (Some(h), Some(p)) => Success(Connection(h, p))
      case _ => Failure(new Exception("missing parameter"))
    }

    override def issueRequest(connection: Connection, payload: String): Try[String] =
      if (payload.length < 20) Success(s"request ($payload) has been accepted")
      else Failure(new Exception("invalid payload"))
  }

  type EitherOrMessage[T] = Either[String, T]
  val eitherHttpServiceInstance = new HttpService[EitherOrMessage] {
    override def getConnection(cfg: Map[String, String]): EitherOrMessage[Connection] =
      (for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(h, p)).map(_.asRight[String]).getOrElse("missing parameter".asLeft)

    override def issueRequest(connection: Connection, payload: String): EitherOrMessage[String] =
      if (payload.length < 20) Right(s"request ($payload) has been accepted")
      else Left("invalid payload")
  }

  object optionHttpServiceInstance extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = for {
      h <- cfg.get("host")
      p <- cfg.get("port")
    } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length < 20) Some(s"request ($payload) has been accepted")
      else None
  }

  println(for {
    conn <- optionHttpServiceInstance.getConnection(config)
    resp <- optionHttpServiceInstance.issueRequest(conn, "hello")
  } yield resp)

}
