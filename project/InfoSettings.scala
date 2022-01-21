import sbt._
import Keys._

object InfoSettings {
  type Sett = Def.Setting[_]

  def all = Seq[Sett](versioninfo)

  val versioninfo = (Compile / sourceGenerators) += task{
    val file = (Compile / sourceManaged).value / "info.scala"
    IO.write(file, s"""package argonaut
                     |object Info {
                     |  val version = "${version.value}"
                     |  val name = "argonaut"
                     |}
                     |""".stripMargin)
    Seq(file)
  }


}
