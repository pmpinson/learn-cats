package com.ppinson.learn.cats.abstratmath

import scala.annotation.tailrec

object CustomMonads extends App {

  import cats.Monad
  import cats.syntax.flatMap._

  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity] {

    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Right(x) => x
      case Left(t) => tailRecM(t)(f)
    }
  }


  trait Tree[+T]

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeMonad extends Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(a) => f(a)
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

//    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(v)) => stackRec(f(v))
        case Leaf(Right(v)) => Leaf(v)
        case Branch(l, r) => Branch(stackRec(l), stackRec(r))
      }
      stackRec(f(a))
    }
  }


  val tree: Tree[Int] = Branch(Leaf(10), Branch(Leaf(20), Leaf(30)))
  println(tree.flatMap(x => Leaf(x * 10)))
  println(tree.flatMap(x => Branch(Leaf(x * 2), Leaf(x * 5))))
}
