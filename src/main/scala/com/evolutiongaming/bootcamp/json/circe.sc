import com.evolutiongaming.bootcamp.json.CirceExercises._

basics.jTrue
basics.jObj.spaces2
basics.jArr
basics.jMatrix.spaces2

basics.twinPeaksS3Score
basics.tpCursor.downField("ratings").downN(2).get[Int]("metaScore")

basics.oldGoodTwinPeaks.spaces2

optics.playCount
optics.allGenres
optics.allMembers

semiauto.gigJson
semiauto.albumJson.spaces2

auto.songJson.spaces2

manual.songJson
manual.albumJson.spaces2

custom1.timeWindowJson
custom1.yearJson

snake_case.dieHardJson
snake_case.dieHardJson.as[snake_case.Movie].getOrElse(null)

adt.hhJson
adt.hhJson.noSpaces

import io.circe.syntax._
adt.Movie(9.0).asJson
adt.Youtube(100).asJson

// Simple example of optics:
final case class B(s1: String, s2: String)
final case class A(b: B, n: Int)
val a1 = A(B("Hello", "Scala"), 10)
val a2 = a1.copy(b = a1.b.copy(s2 = "World"))
// Optics
// val a3 = a1.b.modify(_ => "World")
