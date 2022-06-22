import sbt._
import Keys._
import org.ensime.EnsimeKeys._

object ScalaSettings {
  type Sett = Def.Setting[_]

  private[this] val unusedWarnings = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) =>
        Seq("-Ywarn-unused-import")
      case _ =>
        Seq("-Ywarn-unused:imports")
    }
  }

  def Scala211 = "2.11.12"

  lazy val all: Seq[Sett] = Def.settings(
    scalaVersion := Scala211
  , crossScalaVersions := Seq(Scala211, "2.12.16", "2.13.8", "3.1.3")
  , ensimeScalaVersion := Scala211
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
    (c / console / scalacOptions) --= unusedWarnings.value
  )
}
