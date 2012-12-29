import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._

object build extends Build {
  type Sett = Project.Setting[_]

  lazy val commonSettings: Seq[Sett] = Seq(
      organization := "io.argonaut"
    , version := "6.0-SNAPSHOT"
    , crossVersion := CrossVersion.full
    , scalaVersion := "2.9.2"
    , crossScalaVersions := Seq("2.9.2", "2.10.0")
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
          <id>tonymorris</id>
          <name>Tony Morris</name>
          <url>http://tmorris.net</url>
        </developer>
        <developer>
          <id>mth</id>
          <name>Mark Hibberd</name>
          <url>http://mth.io</url>
        </developer>
        <developer>
          <id>seanparsons</id>
          <name>Sean Parsons</name>
        </developer>
      </developers>)
    , scalacOptions <++= scalaVersion map { v =>
        Seq("-deprecation", "-unchecked", "-optimise") ++ (if (v.contains("2.10"))
          Seq("-Yinline-warnings", "-feature", "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps")
        else
          Seq())
      }
  )

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
  , settings = Defaults.defaultSettings ++ commonSettings ++ Seq[Sett](
      name := "argonaut"
        , libraryDependencies ++= Seq(
        ("org.scalaz" %% "scalaz-core" % "7.0.0-M7").changing
      , ("org.scalacheck" %% "scalacheck" % "1.10.0" % "test").cross(CrossVersion.full)
      )
    , libraryDependencies <+= (scalaVersion){ v =>
        val specsVersion = (if (v.contains("2.10")) "1.13" else "1.12.3")
        "org.specs2" %% "specs2" % specsVersion % "test"
      }
    , initialCommands := """
                           |import argonaut._
                           |import scalaz._
                           |import Scalaz._
                           |import Argonaut._
                         """.stripMargin
    )
  )

  val benchmark = Project(
    id = "benchmark"
  , base = file("benchmark")
  , dependencies = Seq(argonaut)
  , settings = Defaults.defaultSettings ++ commonSettings ++ Seq[Sett](
      name := "argonaut-benchmark"
    , fork in run := true
    , libraryDependencies += "com.google.caliper" % "caliper" % "0.5-rc1"
    , javaOptions in run <++= (fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Build.data(cp).mkString(":")) }
    )
  )
}
