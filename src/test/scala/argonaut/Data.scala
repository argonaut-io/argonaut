package argonaut

import scalaz._, Scalaz._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.{frequency, choose, listOfN, value, oneOf}
import Json._
import org.scalacheck.{Gen, Arbitrary}
import scala.util.Random.shuffle

object Data {
  val maxJsonStructureDepth = 3

  val jsonNumberGenerator: Gen[JNumber] = arbitrary[Double].map(number => JNumber(number))

  def isValidJSONCharacter(char: Char): Boolean = !char.isControl && char != '\\' && char != '\"'

  val stringGenerator: Gen[String] = arbitrary[String]

  val jsonStringGenerator: Gen[JString] = stringGenerator.map(string => JString(string))

  val jsonBoolGenerator: Gen[JBool] = oneOf(JBool(true), JBool(false))

  val jsonNothingGenerator: Gen[Json] = value(JNull)

  def jsonArrayItemsGenerator(depth: Int = maxJsonStructureDepth): Gen[Seq[Json]] = listOfN(5, jsonValueGenerator(depth - 1))

  def jsonArrayGenerator(depth: Int = maxJsonStructureDepth): Gen[JArray] = jsonArrayItemsGenerator(depth).map{values => JArray(values.toList)}

  def jsonObjectFieldsGenerator(depth: Int = maxJsonStructureDepth): Gen[Seq[(JString, Json)]] = listOfN(5, arbTuple2(Arbitrary(jsonStringGenerator), Arbitrary(jsonValueGenerator(depth - 1))).arbitrary)

  private def arbImmutableMap[T: Arbitrary, U: Arbitrary]: Arbitrary[Map[T, U]] =
    Arbitrary(Gen.listOf(arbTuple2[T, U].arbitrary).map(_.toMap))

  def jsonObjectGenerator(depth: Int = maxJsonStructureDepth): Gen[JObject] = arbImmutableMap(Arbitrary(arbitrary[String]), Arbitrary(jsonValueGenerator(depth - 1))).arbitrary.map{map =>
    JObject(JsonObject(InsertionMap(map.toSeq: _*)))
  }

  val nonJsonObjectGenerator = oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator, jsonArrayGenerator())

  val jsonObjectOrArrayGenerator = oneOf(jsonObjectGenerator(), jsonArrayGenerator())

  def jsonValueGenerator(depth: Int = maxJsonStructureDepth): Gen[Json] = {
    if (depth > 1) {
      oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator, jsonArrayGenerator(depth - 1), jsonObjectGenerator(depth - 1))
    } else {
      oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator)
    }
  }

  def objectsOfObjectsGenerator(depth: Int = maxJsonStructureDepth): Gen[Json] = {
    if (depth > 1) {
      listOfN(2, arbTuple2(Arbitrary(arbitrary[String]), Arbitrary(objectsOfObjectsGenerator(depth - 1))).arbitrary).map(fields => JObject(JsonObject(InsertionMap(fields: _*))))
    } else {
      oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator)
    }
  }

  val arrayOrObjectAndPathGenerator: Gen[(Seq[String], Json, Json)] = objectsOfObjectsGenerator().map{jsonvalue =>
    def buildPath(currentPath: Seq[String], original: Json, jsonValue: Json): (Seq[String], Json, Json) = {
      jsonValue match {
        case jsonObject: JObject => {
          shuffle(jsonObject.o.toMap.toList.collect{case pair@ (innerString: String, innerValue: Json) => pair}.toList)
            .headOption
            .map{innerPair =>
              buildPath(currentPath :+ innerPair._1, original, innerPair._2)
            }
            .getOrElse((currentPath, original, jsonObject))
        }
        case other => (currentPath, original, other)
      }
    }
    buildPath(Seq(), jsonvalue, jsonvalue)
  }

  implicit def ArbitraryJString: Arbitrary[JString] = Arbitrary(jsonStringGenerator)

  implicit def ArbitraryJNumber: Arbitrary[JNumber] = Arbitrary(jsonNumberGenerator)

  implicit def ArbitraryJArray: Arbitrary[JArray] = Arbitrary(jsonArrayGenerator())

  implicit def ArbitraryJObject: Arbitrary[JObject] = Arbitrary(jsonObjectGenerator())

  implicit def ArbitraryJBool: Arbitrary[JBool] = Arbitrary(jsonBoolGenerator)

  implicit def ArbitraryJson: Arbitrary[Json] = Arbitrary(jsonValueGenerator())

  implicit def ArbitraryJsonObject: Arbitrary[JsonObject] =
    Arbitrary(arbitrary[List[(JsonField, Json)]] map (as => JsonObject(InsertionMap(as: _*))))

  implicit def ArbitraryCursor: Arbitrary[Cursor] = {
    Arbitrary(arbitrary[Json] flatMap (j => {
      val c = +j
      j.arrayOrObject(
        Gen.value(c)
      , _ =>
          for {
            r <- frequency((90, arbitrary[Cursor]), (10, c))
          } yield c.right getOrElse r
      , o =>
          for {
            r <- frequency((90, arbitrary[Cursor]), (10, c))
            q <- frequency((90, oneOf(o.fields)), (10, arbitrary[JsonField]))
          } yield c downField q getOrElse r
      )
    }))
  }

  implicit def ArbitraryVector[A: Arbitrary]: Arbitrary[Vector[A]] =
    Arbitrary(arbitrary[List[A]].map((as: List[A]) =>
      as.map(a => a)(collection.breakOut)))

  case class SometimesNullString(s: String) {
    override def toString = s
  }

  case class SometimesBoolString(s: String) {
    override def toString = s
  }

  implicit val ArbitrarySometimesNullString: Arbitrary[SometimesNullString] =
    Arbitrary(frequency((1, value("null")), (9, arbitrary[String])) map (SometimesNullString(_)))

  implicit val ArbitrarySometimesBoolString: Arbitrary[SometimesBoolString] =
    Arbitrary(frequency((1, value("true")), (1, value("false")), (8, arbitrary[String])) map (SometimesBoolString(_)))

  implicit def ArbitraryScalazEither[A: Arbitrary, B: Arbitrary]: Arbitrary[A \/ B] =
    Arbitrary(arbitrary[Either[A, B]] map (\/.fromEither(_)))

  implicit val ArbitraryPrettyParams: Arbitrary[PrettyParams] = Arbitrary(
    for {
      lbraceLeft <- arbitrary[Int => String]
      lbraceRight <- arbitrary[Int => String]
      rbraceLeft <- arbitrary[Int => String]
      rbraceRight <- arbitrary[Int => String]
      lbracketLeft <- arbitrary[Int => String]
      lbracketRight <- arbitrary[Int => String]
      rbracketLeft <- arbitrary[Int => String]
      rbracketRight <- arbitrary[Int => String]
      commaLeft <- arbitrary[Int => String]
      commaRight <- arbitrary[Int => String]
      colonLeft <- arbitrary[Int => String]
      colonRight <- arbitrary[Int => String]
      preserveOrder <- arbitrary[Boolean]
    } yield PrettyParams(lbraceLeft, lbraceRight, rbraceLeft, rbraceRight, lbracketLeft, lbracketRight, rbracketLeft, rbracketRight, commaLeft, commaRight, colonLeft, colonRight, preserveOrder)
  )
}
