package com.evolutiongaming.bootcamp.typeclass.v2

object QAndAExamples extends App {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A](implicit instance: Semigroup[A]): Semigroup[A] = instance
  }

  implicit class SemigroupOps[A: Semigroup](x: A) {
    def combine(y: A): A = Semigroup[A].combine(x, y)
  }

  // 1.2. Implement Semigroup for Long, String
  implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => x + y

  implicit val intSemigroup: Semigroup[Int] = (x: Int, y: Int) => x + y

  implicit val stringSemigroup: Semigroup[String] = (x: String, y: String) => x + y

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists
  def combineAll[A: Semigroup](xs: List[A]): A = xs.reduce(_ combine _)

  combineAll(List(1, 2, 3)) == 6

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists
  def combineAll[A: Semigroup](xs: List[A], startingElement: A): A =
    xs.foldLeft(startingElement)(_ combine _)

  combineAll(List(1, 2, 3), 0) == 6
  combineAll(List(), 1) == 1

  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example)
  // and extends Semigroup
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly
  }

  // 2.2. Implement Monoid for Long, String
  implicit val longMonoid: Monoid[Long] = new Monoid[Long] {
    override def empty: Long = 0

    override def combine(x: Long, y: Long): Long = x + y
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x + y
  }

  Monoid[Long].empty
  Monoid[String].empty

  // 2.3. Implement combineAll(list: List[A]) for all lists
  def combineAllMonoid[A: Monoid](xs: List[A]): A = xs.foldLeft(Monoid[A].empty)((a, b) => a.combine(b))

  combineAllMonoid(List(1L, 2, 3)) == 6

  // 2.4. Implement Monoid for Option[A]
  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None

    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(x), Some(y)) => Some(x.combine(y))
      case (x, y) => x.orElse(y)
    }
  }

  optionMonoid[Long].combine(Some(2), None) == Some(2)
  optionMonoid[Long].combine(None, Some(2)) == Some(2)

  combineAllMonoid[Option[Long]](List(Some(1), None, Some(3))) == Some(4)
  combineAllMonoid[Option[Long]](List(None, None)) == None
  combineAllMonoid[Option[Long]](List()) == None

  // 2.5. Implement Monoid for Function1 (for result of the function)
  implicit def functionMonoid[A, B: Monoid]: Monoid[A => B] = new Monoid[A => B] {
    override def empty: A => B = _ => Monoid[B].empty

    override def combine(x: A => B, y: A => B): A => B = a => Monoid[B].combine(x(a), y(a))
  }

  combineAllMonoid[String => Long](List((a: String) => a.length, (a: String) => a.toInt)) // == (a: String) => (a.length + a.toInt)
  combineAllMonoid[String => Long](List((a: String) => a.length, (a: String) => a.toInt)).apply("123") // == 126

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_] : Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def mapFunctor[K]: Functor[Map[K, *]] = new Functor[Map[K, *]] {
    override def fmap[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.transform((_, v) => f(v))
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_] : Semigroupal]: Semigroupal[F] = implicitly
  }

  implicit class SemigroupalOps[F[_] : Semigroupal, A](fa: F[A]) {
    def product[B](fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
  }

  // 4.2. Implement Semigroupal for Option
  implicit val optionSemigroupal: Semigroupal[Option] = new Semigroupal[Option] {
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(a), Some(b)) => Some((a, b))
      case (_, _) => None
    }
  }

  Option(1).product(Some("3"))

  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]
  implicit class Tuple2Ops[F[_] : Functor : Semigroupal, A, B](tuple: Tuple2[F[A], F[B]]) {
    def mapN[R](f: (A, B) => R): F[R] = tuple match {
      case (fa, fb) => fa.product(fb).fmap { case (a, b) => f(a, b) }
    }
  }

  (Option(1), Option("Hello")).mapN(_ + _)
  (Option(1), Option(2)).mapN(_ + _) == Some(3)
  (Option(1), Option.empty[String]).mapN(_ + _) == None

  // 4.4. Implement Semigroupal for Map
  // def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  implicit def mapSemigroupal[K]: Semigroupal[Map[K, *]] = new Semigroupal[Map[K, *]] {
    override def product[A, B](fa: Map[K, A], fb: Map[K, B]): Map[K, (A, B)] =
      fa.foldLeft(Map.empty[K, (A, B)]) { (acc, elem) =>
        val (k, a) = elem
        fb.get(k) match {
          case Some(b) => acc + (k -> (a, b))
          case None => acc
        }
      }
  }

  // TODO: why I get error here?
  // type mismatch;
  // found   : scala.collection.immutable.Map[Int,String]
  // required: scala.math.Numeric[?]
  // println(Map(1 -> "a", 2 -> "b").product(Map(2 -> "c")))

  // If we want to use mapN our Map should be Functor and Semigroupal
  // F[_] : Functor : Semigroupal
  println((Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _)) // == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  // 5.1. Implement Applicative for Option, Either
  new Applicative[Either[String, *]] {
    override def pure[A](x: A): Either[String, A] = Right(x)

    override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] = ???

    override def fmap[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = ???
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???

  // traverse(List(1, 2, 3)) { i =>
  //   Option.when(i % 2 == 1)(i)
  // } == None

  // traverse(List(1, 2, 3)) { i =>
  //   Some(i + 1)
  // } == Some(List(2, 3, 4))

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library

  // 6.3. Implement `traverse` for all Foldables instead of List
}
