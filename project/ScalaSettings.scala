import sbt._
import Keys._
import Tools.onVersionTask

object ScalaSettings {
  type Sett = Def.Setting[_]

  private[this] val unusedWarnings = (
    "-Ywarn-unused" ::
    "-Ywarn-unused-import" ::
    Nil
  )

  def Scala211 = "2.11.12"

  lazy val all: Seq[Sett] = Seq(
    scalaVersion := Scala211
  , crossScalaVersions := Seq("2.10.6", Scala211, "2.12.4")
  , fork in test := true
  , scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:_", "-Xlint", "-Xfuture")
  , scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){
      case Some((2, v)) if v >= 11 => unusedWarnings
    }.toList.flatten
  , unmanagedSourceDirectories in Compile ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 11 =>
          val base = baseDirectory.value.getParentFile / "shared/src/main"
          Seq(base / "scala-2.11+")
        case _ =>
          Nil
      }
    }
  ) ++ Seq(Compile, Test).flatMap(c =>
    scalacOptions in (c, console) ~= {_.filterNot(unusedWarnings.toSet)}
  )
}
