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
          "wiggle" : true,
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
          "xxyyzz" : 36
        }
      """

    val r1 = shift.when(_ hasField "wiggle").downField("xyz")
    val r2 = shift.downField("values").downArray.downArray.right.withFocus(jStringL =>= (_.reverse)).up.right
    val r = (r1 ||| r2) := jString("cat")
    val s = r <| j.pparse
    s.cursor map (c => (-c).spaces2) foreach println
    s.println
  }
}
