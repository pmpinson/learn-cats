package com.ppinson.learn.cats.effects.effects

import cats.effect.{IO, IOApp}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object Brackets extends IOApp.Simple {

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readLines(scanner: Scanner): IO[Unit] =
//    if (scanner.hasNextLine) IO(scanner.nextLine()).flatMap(x => IO.println(x)).flatMap(_=> IO.sleep(100.millis)).flatMap(_ => readLines(scanner))
    if (scanner.hasNextLine) IO(scanner.nextLine()).flatMap(r => IO.println(r)) >> IO.sleep(100.millis) >> readLines(scanner)
    else IO.unit

  override def run: IO[Unit] = openFileScanner("/Users/ppinson/work/test/learn-cats/src/main/scala/com/ppinson/learn/cats/effects/effects/Brackets.scala")
    .bracket(readLines)(scanner => IO(scanner.close()).void)
}
