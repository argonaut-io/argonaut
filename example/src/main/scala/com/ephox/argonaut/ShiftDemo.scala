package com.ephox
package argonaut

import Argonaut._
import scalaz._, Scalaz._

object ShiftDemo {
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
                      [
                        "horse"
                      , "lolo"
                      , [
                          "hi"
                        , "there"
                        ]
                      ]
                    , "dog"
                    , "rabbit"
                    ],
          "xyz" : 24,
        }
      """
    val q = j.parseIgnoreErrorType(
      s => {
        // if has field "wiggle", go to field "xyz"
        val r1 = shift.when(_ hasField "wiggle").downField("xyz")
        // down to field "values", into array, into array, right on array, reverse string at focus, up, right on array
        val r2 = shift.downField("values").downArray.downArray.right.withFocus(jStringL =>= (_.reverse)).up.right
        // try r1, otherwise r2, then set focus to string "cat"
        val r = (r1 ||| r2) := jString("cat")
        // apply cursor-shift operations
        val t = r <| s
       // run cursor-shift and pretty-print with two-space indent
        t.cursor match {
          case None => "no cursor"
          case Some(c) => (-c).spaces2
        }
      }
    , "Failed parse: " + _
    )

    println(q)
  }
}
