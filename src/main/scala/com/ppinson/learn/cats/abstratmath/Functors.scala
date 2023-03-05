package com.ppinson.learn.cats.abstratmath

object Functors extends App {

  import cats.Functor
  import cats.syntax.functor._

  trait Tree[+T]

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  def to10X[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }
  }

  val t1: Tree[Int] = Leaf(12)
  val t2: Tree[Int] = Branch(7, Leaf(12), Branch(13, Leaf(15), Leaf(17)))
  println(to10X(t1))
  println(to10X[Tree](Leaf(12)))
  println(to10X(t2))

  println(t1.map(_ * 10))

  def to10XShort[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  println(to10XShort(t1))
  println(to10XShort(t2))
}
