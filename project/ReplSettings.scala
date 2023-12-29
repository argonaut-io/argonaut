import sbt._
import Keys._

object ReplSettings {
  type Sett = Def.Setting[?]

  lazy val all = Seq[Sett](
    initialCommands := """
                         |import argonaut._
                         |import Argonaut._
                       """.stripMargin
  )
}
