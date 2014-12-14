import sbt._
import Keys._
import Tools.onVersionTask

object ScalaSettings {
  type Sett = Def.Setting[_]

  lazy val all: Seq[Sett] = Seq(
    scalaVersion := "2.11.4"
  , crossScalaVersions := Seq("2.10.4", "2.11.4")
  , fork in test := true
  , scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:_", "-Xlint")
  // https://gist.github.com/djspiewak/976cd8ac65e20e136f05
  , unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / s"scala-${scalaBinaryVersion.value}"
  )
}
