import sbt._
import Keys._
import Tools.onVersionTask
import org.ensime.EnsimeKeys._

object ScalaSettings {
  type Sett = Def.Setting[_]

  private[this] val unusedWarnings = (
    "-Ywarn-unused" ::
    Nil
  )

  def Scala211 = "2.11.12"

  lazy val all: Seq[Sett] = Def.settings(
    scalaVersion := Scala211
  , crossScalaVersions := Seq(Scala211, "2.12.8", "2.13.0-M5")
  , ensimeScalaVersion := Scala211
  , fork in test := true
  , scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:_", "-Xlint", "-Xfuture")
  , scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){
      case Some((2, v)) if v >= 11 => unusedWarnings
    }.toList.flatten
  ) ++ Seq(Compile, Test).flatMap(c =>
    scalacOptions in (c, console) ~= {_.filterNot(unusedWarnings.toSet)}
  )
}
