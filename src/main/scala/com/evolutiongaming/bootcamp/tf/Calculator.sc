/*
  Additional materials:

  Papers by Oleg Kiselyov http://okmij.org/ftp/tagless-final/index.html
  (Haskell, 18+)

  Что такое tagless final? https://www.youtube.com/watch?v=ZNK57IXgr3M
  (Scala 3, история развития кодировок от Черча до TF)

  Tagless Final lessons series https://www.youtube.com/watch?v=XJ2NjqkWdck&list=PLJGDHERh23x-3_T3Dua6Fwp4KlG0J25DI

  Practical FP in Scala https://leanpub.com/pfp-scala (Chapters 3, 4)
*/

import cats._
import cats.syntax.all._


sealed trait Expression

final case class Const(x: Int) extends Expression
final case class Add(left: Expression, right: Expression) extends Expression
final case class Multiply(left: Expression, right: Expression) extends Expression

//  2 * 3 + 4
val expression = Add(
  Multiply(Const(2), Const(3)),
  Const(4)
)

def evaluate(expression: Expression): Int = expression match {
  case Const(n)              => n
  case Add(left, right)      => evaluate(left) + evaluate(right)
  case Multiply(left, right) => evaluate(left) * evaluate(right)
}

def show(expression: Expression): String = expression match {
  case Const(n)              => n.toString
  case Add(left, right)      => show(left) + " + " + show(right)
  case Multiply(left, right) => show(left) + " * " + show(right)
}

show(expression)

sealed trait ExtExpression
final case class Divide(left: Expression, right: Expression) extends ExtExpression

// (10 + 5) / 5
val expression2 = Divide(
  Add(Const(10), Const(5)),
  Const(5)
)

// TODO: does not work
// type mismatch;
// found   : Divide
// required: Expression
// 10 / 5 * 5
//val expression3 = Multiply(
//  Divide(Const(10), Const(5)),
//  Const(5)
//)

trait ExpressionA[A] {
  def const(n: Int): A
  def add(left: A, right: A): A
  def multiply(left: A, right: A): A
}

val intAlgebra = new ExpressionA[Int] {
  override def const(n: Int) = n
  override def add(left: Int, right: Int) = left + right
  override def multiply(left: Int, right: Int) = left * right
}

val stringAlgebra = new ExpressionA[String] {
  override def const(n: Int) = n.toString
  override def add(left: String, right: String) = s"$left + $right"
  override def multiply(left: String, right: String) = s"$left * $right"
}

//  2 * 3 + 4
def expression4[A](algebra: ExpressionA[A]): A = {
  import algebra._

  add(
    multiply(const(2), const(3)),
    const(4)
  )
}

expression4(intAlgebra)
expression4(stringAlgebra)

trait ExtExpressionA[A] {
  def divide(left: A, right: A): A
}

// Divided by zero
// If we using Option[Int], we should update other Expression for using with Option
val intExtAlgebra = new ExtExpressionA[Int] {
  override def divide(left: Int, right: Int) = left / right
}

val stringExtAlgebra = new ExtExpressionA[String] {
  override def divide(left: String, right: String) = s"$left / $right"
}

def expression5[A](algebra: ExpressionA[A], extAlgebra: ExtExpressionA[A]): A = {
  import algebra._, extAlgebra._

  multiply(
    divide(const(6), const(3)),
    const(2)
  )
}

expression5(intAlgebra, intExtAlgebra)
expression5(stringAlgebra, stringExtAlgebra)

val freeAlgebra = new ExpressionA[Expression] {
  override def const(n: Int) = Const(n)
  override def add(left: Expression, right: Expression) = Add(left, right)
  override def multiply(left: Expression, right: Expression) = Multiply(left, right)
}

expression4(freeAlgebra)


trait ExpressionTF[F[_], A] {
  def const(x: A): F[A]
  def add(left: F[A], right: F[A]): F[A]
  def multiply(left: F[A], right: F[A]): F[A]
  def divide(left: F[A], right: F[A]): F[A]
}

object ExpressionTF {
  def apply[F[_], A]: ExpressionTF[F, A] = ???

  def intAlg[F[_]: Monad: MonoidK]: ExpressionTF[F, Int] = new ExpressionTF[F, Int] {
    override def const(x: Int) = x.pure[F]

    override def add(left: F[Int], right: F[Int]) =
      (left, right).mapN(_ + _)
      // left.flatMap(a => right.map(b => a + b))

    override def multiply(left: F[Int], right: F[Int]) =
      (left, right).mapN(_ + _)
      // left.flatMap(a => right.map(b => a * b))

    override def divide(left: F[Int], right: F[Int]) =
      (left, right).tupled.flatMap {
        case (_, y) if y == 0 => MonoidK[F].empty
        case (x, y) => (x / y).pure[F]
      }
  }
}

val dsl = ExpressionTF.intAlg[Option]
import dsl._

add(
  const(3),
  divide(const(6), const(0))
)
