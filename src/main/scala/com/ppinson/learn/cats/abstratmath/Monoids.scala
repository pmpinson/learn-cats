package com.ppinson.learn.cats.abstratmath

object Monoids extends App {

  import cats.Monoid
  import cats.syntax.monoid._
  import cats.instances.map._
  import cats.instances.list._

  def combineFold[T](list: List[T])(implicit M: Monoid[T]): T = list.fold(M.empty)(_ |+|_)
  def combineFoldShort[T: Monoid](list: List[T]): T = list.fold(Monoid[T].empty)(_ |+|_)

//  val phoneBooksMonoid: Monoid[Map[String, Int]] = Monoid.instance(Map.empty, (a, b) => {
//    a ++ b
//  })
//  val phoneBooksMonoid2: Monoid[Map[String, Double]] = Monoid.instance(Map.empty, (a, b) => {
//    a |+| b
//  })

  println(combineFold(List(Map(("phone1" -> 12), ("phone2" -> 14)), Map("phone1" -> 13, ("phone3" -> 1)))))
//  println(combineFold(List(Map(("phone1" -> 12d), ("phone2" -> 14d)), Map("phone1" -> 13d, ("phone3" -> 1d)))))

  case class ShoppingCard(items: List[String], total: Double)

  implicit val shoppingCardMonoid: Monoid[ShoppingCard] = Monoid.instance(ShoppingCard(List(), 0), (a, b) => {
    ShoppingCard(a.items |+| b.items, a.total |+| b.total)
  })

  def checkout(shoppingCards: List[ShoppingCard]): ShoppingCard = combineFold(shoppingCards)

  val shoppingCards = List(
    ShoppingCard(List("casque", "casque"), 20),
    ShoppingCard(List("casque", "sacoche"), 30),
    ShoppingCard(List("ordi", "sacoche"), 1020),
  )
  println(checkout(shoppingCards))
}
