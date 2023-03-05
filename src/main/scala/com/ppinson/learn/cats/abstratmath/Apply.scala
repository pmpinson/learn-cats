package com.ppinson.learn.cats.abstratmath

import cats.{Functor, Semigroup, Semigroupal}

object Apply extends App {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionW: W[B => (A, B)] = map(fa)(a => b => (a, b))
      ap(functionW)(fb)
    }

//    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] =
//      map(tuple)(x => f(x._1, x._2))
//
//
    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T]
  }

}
