import sbt._
import Keys._

object build extends Build {
  type Sett = Project.Setting[_]

  lazy val publishSetting = publishTo <<= (version).apply(v =>
    Some(Resolver.sftp(
      "repo.mth.io",
      "repo.mth.io",
      "repo.mth.io/data/" + (if (v.trim.endsWith("SNAPSHOT")) "snapshots" else "releases")
    ) as (
      "web", new java.io.File(System.getProperty("user.home") + "/.ssh/repo_mth_publish")
    )))

  val argonaut = Project(
    id = "argonaut"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "argonaut"
    , organization := "org.argonaut"
    , version := "6.0-SNAPSHOT"
    , crossVersion := CrossVersion.full
    , scalaVersion := "2.9.2"
    , crossScalaVersions := Seq("2.9.2", "2.10.0-RC5")
    , publishSetting
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        ("org.scalaz" %% "scalaz-core" % "7.0.0-M6").cross(CrossVersion.full).changing
      , ("org.scalacheck" %% "scalacheck" % "1.10.0" % "test").cross(CrossVersion.full)
      )
    , initialCommands := """
                           |import argonaut._
                           |import scalaz._
                           |import Scalaz._
                           |import Json._
                         """.stripMargin
    )
  )

  val example = Project(
    id = "example"
  , base = file("example")
  , dependencies = Seq(argonaut)
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "example"
    , organization := "argonaut"
    , version := "1.0"
    , scalaVersion := "2.9.2"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        ("org.scalaz" %% "scalaz-core" % "7.0.0-M6").cross(CrossVersion.full).changing
      , ("org.scalacheck" %% "scalacheck" % "1.10.0" % "test").cross(CrossVersion.full)
      )
    )
  )
}
