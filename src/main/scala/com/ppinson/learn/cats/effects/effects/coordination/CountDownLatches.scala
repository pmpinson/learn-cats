package com.ppinson.learn.cats.effects.effects.coordination

import cats.effect.kernel.{Deferred, Ref}
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.parallel._
import cats.syntax.traverse._
import com.ppinson.learn.cats.effects.effects.IOExtension

import java.io.{File, FileInputStream, FileWriter}
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Random

object CountDownLatches extends IOApp.Simple {

  object FileServer {
    val fileChunkList: List[String] = List(
      "I love Scala",
      "Cats Effect seems quite fun",
      "Never would I have done low level",
    )

    def getNumChunks: IO[Int] = IO(fileChunkList.size)

    def getFileChunk(n: Int): IO[String] = IO(fileChunkList(n))
  }

  def writeToFile(path: String, contents: String): IO[Unit] =
    Resource
      .make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
      .use(writer => IO(writer.write(contents)))

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] =
    (for {
      reader <- Resource.make(IO(new FileInputStream(new File(fromPath))))(reader => IO(reader.close()))
      writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
    } yield (reader, writer)).use { case (reader, writer) =>
      IO(Source.fromInputStream(reader).getLines().foreach(v => writer.write(v + "\n")))
    }

  def downloadFile(fileName: String, destFolder: String): IO[Unit] = for {
    _ <- IO("[main] Retrieve number of chunks").debug
    nbChunks <- FileServer.getNumChunks
    //    latch <- CountDownLatch[IO](nbChunks)
    latch <- MyLatch(nbChunks)
    _ <- (0 until nbChunks).toList
      .parTraverse { idx =>
        for {
          _ <- IO(s"[chunk $idx] Download").debug
          chunk <- FileServer.getFileChunk(idx)
          _ <- IO.sleep(Random.nextInt(10).second)
          file = s"$destFolder/$idx.chunk"
          _ <- writeToFile(file, chunk)
          _ <- IO(s"[chunk $idx] written to $file").debug
          _ <- latch.release
        } yield file
      }.start
    _ <- IO("[main] wait for download to finish").debug
    _ <- latch.await
    _ <- IO("[main] Download finish, consolidating").debug
    _ <- (0 until nbChunks).toList.map(idx => appendFileContents(s"$destFolder/$idx.chunk", fileName)).sequence
  } yield ()

  sealed trait MyLatchState

  case object Done extends MyLatchState

  case class Live(count: Int, signal: Deferred[IO, Unit]) extends MyLatchState

  class MyLatch(state: Ref[IO, MyLatchState]) {

    def await: IO[Unit] = state.get.flatMap {
      case Done => IO.unit
      case Live(_, signal) => signal.get
    }

    def release: IO[Unit] = state.modify {
      case Done => (Done, IO.unit)
      case Live(1, signal) => (Done, signal.complete(()).void)
      case Live(cur, signal) => (Live(cur - 1, signal), IO.unit)
    }.flatten
  }

  object MyLatch {

    def apply(count: Int): IO[MyLatch] = for {
      signal <- Deferred[IO, Unit]
      state <- Ref[IO].of[MyLatchState](Live(count, signal))
    } yield new MyLatch(state)
  }

  override def run: IO[Unit] = downloadFile("data/res.txt", "data/chunks")
}
