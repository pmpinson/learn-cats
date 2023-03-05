package com.ppinson.learn.cats.abstratmath

object Semigroups extends App {

  import cats.Semigroup
  import cats.syntax.semigroup._

  def reduceThings[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  case class Expense(id: Long, amount: Double)

  //  implicit object ExpenseSemiGroup extends Semigroup[Expense] {
  //    override def combine(x: Expense, y: Expense): Expense = Expense(0, x.amount + y.amount)
  //  }

  implicit val expenseSemiGroup: Semigroup[Expense] = Semigroup.instance {
    (x: Expense, y: Expense) => Expense(-1, x.amount + y.amount)
  }

  val expenses = List(
    Expense(1, 12.5),
    Expense(10, 47.5),
  )

  println(reduceThings(expenses))

  println("12".combineN(12))
}
