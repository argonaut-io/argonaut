package argonaut

import argonaut._, Argonaut._
import scalaz._, Scalaz._

object CursorDemo {
  def main(args: Array[String]) {
    val j =
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

    val c1 =
      j.pparse flatMap (k =>
        +k --\ "values" flatMap (_.downArray) map (_ := jBool(false)) map (-_)
      )

    val c2 =
      j.pparse flatMap (k =>
        +k --\ "values" flatMap (_.downArray) map (-_)
      )

    val c3 =
      j.pparse flatMap (k =>
        +k --\ "values" flatMap (_.downArray) flatMap (_.right) flatMap (!_) map (-_)
      )

    val c4 =
      j.pparse flatMap (k =>
        +k --\ "values" flatMap (_.downArray) map (_ := jBool(false)) flatMap (_.right) flatMap (_.right) map (_ := jBool(true)) map (-_)
      )

    List(c1, c2, c3, c4) map (_ map (_.spaces2)) foreach println
  }
}
