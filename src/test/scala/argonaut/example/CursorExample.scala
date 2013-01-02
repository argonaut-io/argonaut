package argonaut.example

import argonaut._, Argonaut._
import scalaz._, Scalaz._
import org.specs2._
import org.specs2.specification._

object CursorExample extends Specification {
  val json =
      """
        {
          "abc" :
            {
              "def" : 7
            },
          "ghi" :
            {
              "ata" : null,
              "jkl" :
                {
                  "mno" : "argo"
                }
            },
          "pqr" : false,
          "operator": "is",
          "values": [
                      ["cat", "lol"]
                    , "dog"
                    , "rabbit"
                    ],
          "xyz" : 24
        }
      """

  def is = "CursorExample" ^
    """Replace '["cat", "lol"]' with 'false'""" ! {
      json.parseOption flatMap (k =>
        +k --\ "values" flatMap (_.downArray) map (_ := jBool(false)) map (-_)
      ) must beSome
    } ^
    "Visit the 'values' array" ! {
      json.parseOption flatMap (k =>
        +k --\ "values" flatMap (_.downArray) map (-_)
      ) must beSome
    } ^
    """Delete the element '"dog"' from the 'values' array.""" ! {
      json.parseOption flatMap (k =>
        +k --\ "values" flatMap (_.downArray) flatMap (_.right) flatMap (!_) map (-_)
      ) must beSome
    } ^
    """Replace '["cat", "lol"]' with 'false' and '"rabbit"' with 'true'""" ! {
      json.parseOption flatMap (k =>
        +k --\ "values" flatMap (_.downArray) map (_ := jBool(false)) flatMap (_.right) flatMap (_.right) map (_ := jBool(true)) map (-_)
      ) must beSome
    }
}
