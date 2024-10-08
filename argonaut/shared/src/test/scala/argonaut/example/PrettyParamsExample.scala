package argonaut.example

import argonaut.TestCompat.*
import argonaut.*
import Argonaut.*

object PrettyParamsExample extends ArgonautSpec {

  case class Address(street: String, number: Int, unit: Option[Int])
  case class Person(name: String, age: Int, address: Option[Address], favouriteNumbers: Option[List[Int]])

  implicit val AddressCodecJson: CodecJson[Address] =
    casecodec3(Address.apply, (_: Address).asTuple)("street", "number", "unit")

  implicit val PersonCodecJson: CodecJson[Person] =
    casecodec4(Person.apply, (_: Person).asTuple)("name", "age", "address", "favouriteNumbers")

  val person = Person("fred", 23, Some(Address("street", 123, None)), Some(List(1, 2, 3)))
  val personNoFavouriteNumbers = person.copy(favouriteNumbers = None)
  val personEmptyFavouriteNumbers = person.copy(favouriteNumbers = Some(Nil))

  val prettyParams = PrettyParams.spaces2.copy(preserveOrder = true)

  val defaultPrettyParams2Spaces = prettyParams
  val defaultPrettyParams2SpacesJson = """
  |{
  |  "name" : "fred",
  |  "age" : 23,
  |  "address" : {
  |    "street" : "street",
  |    "number" : 123,
  |    "unit" : null
  |  },
  |  "favouriteNumbers" : [
  |    1,
  |    2,
  |    3
  |  ]
  |}
  """.trim.stripMargin

  val defaultPrettyParams2SpacesNoSpaceBeforeColon = prettyParams.copy(colonLeft = "")
  val defaultPrettyParams2SpacesNoSpaceBeforeColonJson = """
  |{
  |  "name": "fred",
  |  "age": 23,
  |  "address": {
  |    "street": "street",
  |    "number": 123,
  |    "unit": null
  |  },
  |  "favouriteNumbers": [
  |    1,
  |    2,
  |    3
  |  ]
  |}
  """.trim.stripMargin

  val prettyParamsDropNullKeys = prettyParams.copy(dropNullKeys = true)
  val prettyParamsDropNullKeysJson = """
  |{
  |  "name" : "fred",
  |  "age" : 23,
  |  "address" : {
  |    "street" : "street",
  |    "number" : 123
  |  }
  |}
  """.trim.stripMargin

  val prettyParamsArrayElementsOnSameLine = prettyParams.copy(arrayCommaRight = " ")
  val prettyParamsArrayElementsOnSameLineJson = """
  |{
  |  "name" : "fred",
  |  "age" : 23,
  |  "address" : {
  |    "street" : "street",
  |    "number" : 123,
  |    "unit" : null
  |  },
  |  "favouriteNumbers" : [
  |    1, 2, 3
  |  ]
  |}
  """.trim.stripMargin

  val prettyParamsEmptyArray = prettyParams.copy(lrbracketsEmpty = "")
  val prettyParamsEmptyArrayJson = """
  |{
  |  "name" : "fred",
  |  "age" : 23,
  |  "address" : {
  |    "street" : "street",
  |    "number" : 123,
  |    "unit" : null
  |  },
  |  "favouriteNumbers" : []
  |}
  """.trim.stripMargin

  def is = s2"""
  Can print default pretty params with 2 spaces ${person.asJson.pretty(
      defaultPrettyParams2Spaces
    ) must_== defaultPrettyParams2SpacesJson}
  Can print default pretty params with 2 spaces no space before colon ${person.asJson.pretty(
      defaultPrettyParams2SpacesNoSpaceBeforeColon
    ) must_== defaultPrettyParams2SpacesNoSpaceBeforeColonJson}
  Can print default pretty params with 2 spaces with no null keys ${personNoFavouriteNumbers.asJson.pretty(
      prettyParamsDropNullKeys
    ) must_== prettyParamsDropNullKeysJson}
  Can print default pretty params with 2 spaces with array elements on the same line ${person.asJson.pretty(
      prettyParamsArrayElementsOnSameLine
    ) must_== prettyParamsArrayElementsOnSameLineJson}
  Can print default pretty params with 2 spaces with no gap for empty arrays ${personEmptyFavouriteNumbers.asJson
      .pretty(prettyParamsEmptyArray) must_== prettyParamsEmptyArrayJson}
  """
}
