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

  def sourceDirectoryName(version: String): String =
    CrossVersion.partialVersion(version) match {
      case Some((major, minor)) =>
        s"scala-$major.$minor"
    }

  lazy val all: Seq[Sett] = Seq(
    scalaVersion := "2.11.8"
  , crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.0-RC2")
  , fork in test := true
  , scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:_", "-Xlint")
  , scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){
      case Some((2, v)) if v >= 11 => unusedWarnings
    }.toList.flatten
  , unmanagedSourceDirectories in Compile +=
      (sourceDirectory in Compile).value / sourceDirectoryName(scalaVersion.value)
  ) ++ Seq(Compile, Test).flatMap(c =>
    scalacOptions in (c, console) ~= {_.filterNot(unusedWarnings.toSet)}
  )
}
