import sbt.*
import Keys.*

object ReplSettings {
  type Sett = Def.Setting[?]

  lazy val all = Seq[Sett](
    initialCommands := """
                         |import argonaut._
                         |import Argonaut._
                       """.stripMargin
  )
}
