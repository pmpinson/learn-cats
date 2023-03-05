package com.ppinson.learn.cats.abstratmath

import cats.Apply

object WeakerMonad extends App {

  trait MyFlatMap[M[_]] extends Apply[M] {

    def flatMap[A, B](mA: M[A])(f: A => M[B]): M[B]

    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wf)(x => map(wa)(a => x(a)))
  }
}
