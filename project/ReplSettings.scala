import sbt._
import Keys._

object ReplSettings {
  type Sett = Project.Setting[_]

  lazy val all = Seq[Sett](
    initialCommands := """
                         |import argonaut._
                         |import scalaz._
                         |import Scalaz._
                         |import Argonaut._
                       """.stripMargin
  )
}
