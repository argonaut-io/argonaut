package argonaut

import argonaut._, Argonaut._
import scalaz._, Scalaz._
import org.specs2._
import org.specs2.specification._

object JsonInterpolatorSpecification extends Specification {
  val constructedJson =
    Json(
      "name" := "fred",
      "age" := 23,
      "wallet" := List(
        Json( "value" := 100 ),
        Json( "value" := 10 ),
        Json( "value" := 50 )
      )
    )

  val interpolatedJson =
    json"""
          {
            "name": "fred",
            "age": 23,
            "wallet": [
              { "value": 100 },
              { "value": 10 },
              { "value": 50}
            ]
          }
          """.getOrElse(throw new Exception())

  def is = s2"""
    Constructed Json matches interpolated Json
    ${ constructedJson must_== interpolatedJson }
  """
}
