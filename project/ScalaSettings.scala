import sbt._
import Keys._
import Tools.onVersionTask

object ScalaSettings {
  type Sett = Def.Setting[_]

  val on210or211 = Seq("-Yinline-warnings", "-feature", "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps")

  lazy val all: Seq[Sett] = Seq(
    scalaVersion := "2.10.3"
  , crossScalaVersions := Seq("2.9.2", "2.9.3", "2.10.3", "2.11.0-RC1")
  , fork in test := true
  , scalacOptions <++= onVersionTask(
      all = Seq("-deprecation", "-unchecked", "-optimise")
    , on210 = on210or211
    , on211 = on210or211
    )
  )
}
