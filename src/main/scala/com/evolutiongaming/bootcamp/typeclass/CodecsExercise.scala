package com.evolutiongaming.bootcamp.typeclass

object CodecsExercise {
  sealed trait Json {
    def /(key: String): JsonResult = this match {
      case JsonObject(value) => JsonResult(value.get(key))
      case _                 => JsonResult(None)
    }
  }
  final case object JsonNull extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json
  final case class JsonArray(value: List[Json]) extends Json
  final case class JsonObject(value: Map[String, Json]) extends Json

  final case class JsonResult(v: Option[Json]) {
    // TODO: Add for chaining and rewrite example
    def /(key: String): JsonResult = ???
    def as[A: Decoder]: Option[A] = v.flatMap(Decoder[A].fromJson)
  }

  // Encoder
  trait Encoder[A] { self =>
    def toJson(a: A): Json

    // Extract to trait Contravariant
    def contramap[B](f: B => A): Encoder[B] = (b: B) => self.toJson(f(b))
  }

  // Encoder summoner
  object Encoder {
    def apply[A: Encoder]: Encoder[A] = implicitly[Encoder[A]]
  }

  // Encoder syntax
  implicit class EncoderOps[A: Encoder](a: A) {
    def toJson: Json = Encoder[A].toJson(a)
  }

  // TODO: add functor from HKT

  // Decoder
  trait Decoder[A] { self =>
    def fromJson(json: Json): Option[A]

    def map[B](a: A)(f: A => B): Decoder[B] = (json: Json) => self.fromJson(json).map(f)
  }

  object Decoder {
    def apply[A: Decoder]: Decoder[A] = implicitly[Decoder[A]]
  }

  implicit class DecoderOps(json: Json) {
    def as[A: Decoder]: Option[A] = Decoder[A].fromJson(json)
  }

  implicit val decoderFunctor: Functor[Decoder] = new Functor[Decoder] {
    override def fmap[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.fromJson(_).map(f)
  }


  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_], A](fa: F[A])(implicit functor: Functor[F]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  // Exercise 1. Implement Encoder and Decoder for Int.
  implicit val IntEncoder: Encoder[Int] = (a: Int) => JsonInt(a)
  implicit val IntDecoder: Decoder[Int] = {
    case JsonInt(value) => Some(value)
    case _ => None
  }

  100.toJson
  JsonNull.as[Int]

  // Exercise 2. Implement Encoder and Decoder for String.
  implicit val StringEncoder: Encoder[String] = (s: String) => JsonString(s)
  implicit val StringDecoder: Decoder[String] = (json: Json) => Some(json).collect {
    case JsonString(value) => value
  }

  "Example".toJson
  JsonNull.as[String]


  final case class Person(name: String, age: Int)

  // Exercise 3. Implement Encoder and Decoder for Person.
  implicit val PersonEncoder: Encoder[Person] = (p: Person) => JsonObject(
    Map(
      "name" -> p.name.toJson,
      "age" -> p.age.toJson
    )
  )
  implicit val PersonDecoder: Decoder[Person] = {
    case JsonObject(values) => for {
      name <- values.get("name").flatMap(_.as[String])
      age <- values.get("age").flatMap(_.as[Int])
    } yield Person(name, age)
    case _ => None
  }

  Person("Ivan", 25).toJson
  JsonNull.as[Person]

  // Exercise 4. Implement Encoder and Decoder for List with any content.
  implicit def listEncoder[A: Encoder]: Encoder[List[A]] = ???
  implicit def listDecoder[A: Decoder]: Decoder[List[A]] = ???


  final case class EntityId(id: String) extends AnyVal

  // Exercise 5. Implement Encoder and Decoder for EntityId with any content.
  implicit val idEncoder: Encoder[EntityId] = Encoder[String].contramap[EntityId](_.id)
//  implicit val idEncoder: Encoder[EntityId] = (entityId: EntityId) =>
//    JsonObject(Map("entityId" -> entityId.id.toJson))
//  implicit val idDecoder: Decoder[EntityId] = {
//    case JsonObject(values) => values.get("entityId").flatMap(_.as[String]).map(EntityId)
//    case _ => None
//  }

  // TODO: replace map with fmap
  implicit val idDecoder: Decoder[EntityId] = Decoder[String].fmap(EntityId)


  // Exercise 6. Describe Functor
  // 1. Typeclass itself: `trait Functor`
  // 2. Typeclass Summoner: `object Functor`
  // 3. Typeclass Ops: `implicit class FunctorOps`

  // Exercise 7. Implement Functor for decoder: `implicit val decoderFunctor`

  // Exercise 8. Describe Contravariant
  // 1. Typeclass itself: `trait Contravariant`
  // 2. Typeclass Summoner: `object Contravariant`
  // 3. Typeclass Ops: `implicit class ContravariantOps`

  // Exercise 9. Implement Contravariant for encoder: `implicit val encoderContravariant`

  // Functions Example
  val foo1: String => Int = _.length
  val foo2: Boolean => String = if (_) "100" else "1"

  // Exercise 10. Implement Functor and Contravariant for functions:
  // implicit def functionFunctor
  // implicit def functionContravariant

  // val foo3: Boolean => Int = functionFunctor.fmap(foo2)(foo1)
  // val foo4: Boolean => Int = functionContravariant.contramap(foo1)(foo2)
}
