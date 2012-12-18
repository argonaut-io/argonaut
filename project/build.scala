import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._

object build extends Build {
  type Sett = Project.Setting[_]


  lazy val publishSetting =
    publishTo <<= version.apply(v => {
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      })

  val argonaut = Project(
    id = "argonaut"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "argonaut"
    , organization := "io.argonaut"
    , version := "6.0-SNAPSHOT"
    , crossVersion := CrossVersion.full
    , scalaVersion := "2.9.2"
    , crossScalaVersions := Seq("2.9.2", "2.10.0-RC5")
    , publishSetting
    , publishMavenStyle := true
    , publishArtifact in Test := false
    , pomIncludeRepository := { _ => false }
    , licenses := Seq("BSD-3-Clause" -> url("http://www.opensource.org/licenses/BSD-3-Clause"))
    , homepage := Some(url("http://argonaut.io"))
    , useGpg := true
    , pomExtra := (
      <scm>
        <url>git@github.com:markhibberd/argonaut.git</url>
        <connection>scm:git:git@github.com:markhibberd/argonaut.git</connection>
      </scm>
      <developers>
        <developer>
          <name>tonymorris</name>
          <url>http://tmorris.net</url>
        </developer>
        <developer>
          <id>mth</id>
          <name>Mark Hibberd</name>
          <url>http://mth.io</url>
        </developer>
        <developer>
          <name>Sean Parsons</name>
        </developer>
      </developers>)
    , scalacOptions <++= scalaVersion map { v =>
        Seq("-deprecation", "-unchecked") ++ (if (v.contains("2.10"))
          Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps")
        else
          Seq())
      }
    , libraryDependencies ++= Seq(
        ("org.scalaz" %% "scalaz-core" % "7.0.0-M6").cross(CrossVersion.full).changing
      , ("org.scalacheck" %% "scalacheck" % "1.10.0" % "test").cross(CrossVersion.full)
      , ("org.specs2" %% "specs2" % "1.12.3" % "test").cross(CrossVersion.full)
      )
    , initialCommands := """
                           |import argonaut._
                           |import scalaz._
                           |import Scalaz._
                           |import Json._
                         """.stripMargin
    )
  )
}
