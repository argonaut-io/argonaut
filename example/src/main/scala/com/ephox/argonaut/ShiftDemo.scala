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

    // if has field "wiggle", go to field "xyz"
    val r1 = shift.when(_ hasField "wiggle").downField("xyz")
    // down to field "values", into array, into array, right on array, reverse string at focus, up, right on array
    val r2 = shift.downField("values").downArray.downArray.right.withFocus(jStringL =>= (_.reverse)).up.right
    // try r1, otherwise r2, then set focus to string "cat"
    val r = (r1 ||| r2) := jString("cat")
    // parse JSON string and apply cursor-shift operations
    val s = r <| j.pparse
    // run cursor-shift and pretty-print with two-space indent
    s.cursor map (c => (-c).spaces2) foreach println
    // print new JSON value and history
    s.println
  }
}
