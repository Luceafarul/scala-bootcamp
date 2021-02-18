package com.evolutiongaming.bootcamp.typeclass.v2

import scala.util.Random

// Implicits: implicit parameters
//            implicit conversions

object ImplicitParameters extends App {

  implicit val name: String = "Oleg"

  LuckService.greet
  LuckService.predictLuck
  LuckService.bye

  trait Animal
  class Cat extends Animal

  case class Person(name: String)(implicit val animal: Animal)

  implicit val cat: Cat = new Cat
  Person("John")
}

object LuckService {
  def greet(implicit name: String): Unit = println(s"Hello $name")
  def predictLuck(implicit name: String): Unit = println(s"Your luck is ${Random.nextInt(11)} today, $name")
  def bye(implicit name: String): Unit = println(s"See you $name")
}

object ImplicitParamTask {
  object Task1 {
    final case class User(id: String)
    trait DbConnection

    object DbConnection {
      def apply(): DbConnection = new DbConnection {}
      implicit val connection: DbConnection = DbConnection()
    }

    // make second argument implicit
    def createUser(user: User)(implicit connection: DbConnection): Unit = ???
    createUser(User("123"))
  }

  object Task2 {
    final case class Money(amount: Int)
    object Money {
      // Add implicit for compile
      implicit val moneyOrdering: Ordering[Money] = (x: Money, y: Money) => ???
    }

    val list: List[Money] = ???

    // oh no, i won't compile
    list.sorted
  }
}
