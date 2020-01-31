import sbt._
import Keys._
import org.ensime.EnsimeKeys._

object ScalaSettings {
  type Sett = Def.Setting[_]

  private[this] val unusedWarnings = Def.setting {
    Seq("-Ywarn-unused:imports")
  }

  def Scala212 = "2.12.10"

  lazy val all: Seq[Sett] = Def.settings(
    scalaVersion := Scala212
  , crossScalaVersions := Seq(Scala212, "2.13.1")
  , ensimeScalaVersion := Scala212
  , fork in test := true
  , scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:_", "-Xlint")
  , scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11 | 12)) =>
          Seq("-Xfuture")
        case _ =>
          Nil
      }
    }
  , scalacOptions ++= unusedWarnings.value
  ) ++ Seq(Compile, Test).flatMap(c =>
    scalacOptions in (c, console) --= unusedWarnings.value
  )
}
