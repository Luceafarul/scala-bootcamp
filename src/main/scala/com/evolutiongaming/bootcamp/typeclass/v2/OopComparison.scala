package com.evolutiongaming.bootcamp.typeclass.v2

import com.evolutiongaming.bootcamp.typeclass.v2.Fp.{Jsonable, User}
import com.evolutiongaming.bootcamp.typeclass.v2.Summoner.Jsonable

final case class Json(s: String) // my very basic json class

object Oop extends App {

  trait Jsonable {
    def toJson: Json
  }

  // my imaginary method which makes use of Jsonable
  def printBeautifully(x: Jsonable): Unit = {
    println(x.toJson)
  }

  // my entity
  final case class User(name: String) extends Jsonable {
    def toJson: Json = Json(s"{name: $name}") // Jsonable implementation for User
  }

  printBeautifully(User("Oleg"))
}

// Typeclass is a trait with type parameter
// the main idea behind typeclass is here
// ask questions if you can't understand what is going on
object Fp extends App {

  trait Jsonable[T] { // <- the typeclass itself: a trait with one type parameter
    //           ^
    //   the type parameter
    def toJson(entity: T): Json // it may have many methods but usually one
  }

  // a typeclass describes an interface which can be implemented for different types

  // my imaginary method changed its signature a bit
  def printBeautifully[A](x: A)(implicit jsonable: Jsonable[A]): Unit = {
    println(jsonable.toJson(x))
  }

  // my entity does not implement anything and can be implemented with no idea Jsonable exists
  final case class User(name: String)

  // here goes the implementation
  // we can define it not touching User or Jsonable or anything
  implicit val userJsonable: Jsonable[User] = new Jsonable[User] {
    def toJson(user: User): Json = Json(s"{name: ${user.name}")
  }

  // the usage is the same
  printBeautifully(User("Oleg"))

  object InstancesTask {

    final case class Player(id: Int, login: String)

    implicit val playerJsonable: Jsonable[Player] =
      (entity: Player) => Json(
        s"""
           |{
           |"id": "${entity.id}",
           |"login": "${entity.login}",
           |}""".stripMargin)

    implicit val intJsonable: Jsonable[Int] = (n: Int) => Json(s"$n")

    implicit val optionIntJsonable: Jsonable[Option[Int]] = {
      case None => Json("")
      case Some(value) => Json(s"$value")
    }
  }

  // you will definitely get it but maybe a bit later and its ok
  object GenericImplicitsTask {
    // the thing can convert to json any options which is super useful
    implicit def optionJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[Option[A]] = {
      case Some(value) => jsonableA.toJson(value)
      case None => Json("null")
    }

    implicit def listJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[List[A]] =
      (entity: List[A]) => Json(s"[${entity.map(jsonableA.toJson)}]")
  }
}

// lets add pieces of sugar one by one
// don't try to remember everything

object SingleAbstractMethod {
  // they are the same, choose any style you like

  implicit val was: Jsonable[User] = new Jsonable[User] {
    def toJson(user: User): Json = Json(s"{name: ${user.name}")
  }

  implicit val now: Jsonable[User] = user => Json(s"{name: ${user.name}")

  // TODO: go back to your InstancesTask and change your instances to SAM
}

object ContextBound {
  def printBeautifullyOld[A](x: A)(implicit jsonable: Jsonable[A]): Unit = {
    println(jsonable.toJson(x))
  }

  def printBeautifully[A: Jsonable](x: A): Unit = {
    val jsonable = implicitly[Jsonable[A]]
    println(jsonable.toJson(x))
  }
}

object Summoner {

  object Jsonable { // but it gets a companion object

    // with nice summon method (could have any name, apply for eg)
    def apply[F](implicit instance: Jsonable[F]): Jsonable[F] = instance
  }

  // so now we can change
  def printBeautifullyOld[A: Jsonable](x: A): Unit = {
    val jsonable = implicitly[Jsonable[A]]
    println(jsonable.toJson(x))
  }

  // to
  def printBeautifully[A: Jsonable](x: A): Unit = {
    println(Jsonable[A].toJson(x))
  }
}

object Syntax {

  object JsonableSyntax {

    implicit class JsonableOps[A](x: A) {
      def toJson(implicit j: Jsonable[A]): Json = {
        j.toJson(x)
      }
    }

  }

  // so now we can change
  def printBeautifullyOld[A: Jsonable](x: A): Unit = {
    println(Jsonable[A].toJson(x))
  }

  // to

  import JsonableSyntax._

  def printBeautifully[A: Jsonable](x: A): Unit = {
    println(x.toJson)
  }
}


object Result {

  // --- json library (provides the typeclass) ---
  trait Jsonable[T] {
    def toJson(entity: T): Json
  }

  object Jsonable {
    def apply[F](implicit instance: Jsonable[F]): Jsonable[F] = instance
  }

  object JsonableSyntax {

    implicit class JsonableOps[A](x: A) {
      def toJson(implicit j: Jsonable[A]): Json = {
        j.toJson(x)
      }
    }

  }

  // --- library which makes use of json (for example some http library) ---

  import JsonableSyntax._

  def printBeautifully[A: Jsonable](x: A): Unit = {
    println(x.toJson)
  }

  // --- domain library of your project ---
  final case class User(name: String)

  // --- domain utils library ---
  implicit val JsonableUser: Jsonable[User] = user => Json(s"{name: ${user.name}") // good luck choosing name

  // --- you ---
  printBeautifully(User("Oleg"))
}

// having two implementations for the same type (like different ways to make json out of User) is possible
// but considered to be bad

object TypeclassTask {

  // Why am I not a Typeclass?
  // TODO: Rework me so I am a typeclass
  trait HashCode {
    def hash: Int
  }

  object HashCode {
    // TODO: Implement me a summoner
  }

  implicit class HashCodeSyntax[A](x: A) {
    // TODO: Implement syntax so I can "abc".hash
  }

  // TODO: make an instance for String
  // TODO: write "abc".hash to check everything
}
