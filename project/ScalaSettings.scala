import sbt._
import Keys._
import Tools.onVersionTask

object ScalaSettings {
  type Sett = Project.Setting[_]

  lazy val all: Seq[Sett] = Seq(
    scalaVersion := "2.10.0"
  , crossScalaVersions := Seq("2.9.2", "2.10.0")
  , fork in test := true
  , scalacOptions <++= onVersionTask(
      all = Seq("-deprecation", "-unchecked", "-optimise")
    , on210 = Seq("-Yinline-warnings", "-feature", "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps")
    )
  )
}
