package com.ppinson.learn.cats.abstratmath

import cats.kernel.Monoid

object Folding extends App {

  object ListExercices {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldLeft(List.empty[B])((a, b) => a :+ f(b))

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldLeft(List.empty[B])((a, b) => a ++ f(b))

    def filter[A](list: List[A])(f: A => Boolean): List[A] = list.foldLeft(List.empty[A])((a, b) => if (f(b)) a :+ b else a)

    def combineAll[A](list: List[A])(implicit m: Monoid[A]): A = list.fold(m.empty)(m.combine)
  }

}
