package com.ppinson.learn.cats.effects

import cats.effect.IO

package object effects {
  implicit class IOExtension[A](io: IO[A]) {
    def debug: IO[A] = for {
      a <- io
      t = Thread.currentThread().getName
      _ = println(s"[$t] $a")
    } yield a
  }
}
