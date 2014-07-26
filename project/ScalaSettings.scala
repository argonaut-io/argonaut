import sbt._
import Keys._
import Tools.onVersionTask

object ScalaSettings {
  type Sett = Def.Setting[_]

  val on210or211 = Seq("-Yinline-warnings", "-feature", "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps")

  lazy val all: Seq[Sett] = Seq(
    scalaVersion := "2.10.3"
  , crossScalaVersions := Seq("2.10.3", "2.11.2")
  , fork in test := true
  , scalacOptions <++= onVersionTask(
      all = Seq("-deprecation", "-unchecked")
    , on210 = on210or211 :+ "-optimise"
    , on211 = on210or211 // https://issues.scala-lang.org/browse/SI-8598
    )
  )
}
