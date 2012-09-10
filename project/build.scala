import sbt._
import Keys._

object build extends Build {
  type Sett = Project.Setting[_]

  override lazy val settings = super.settings ++
        Seq(resolvers := Seq(
          "mth.io snapshots"  at "http://repo.mth.io/snapshots"
        , "mth.io releases"  at "http://repo.mth.io/releases"
        , "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
        , "releases"  at "http://oss.sonatype.org/content/repositories/releases"
        ))

  val argonaut = Project(
    id = "argonaut"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "argonaut"
    , organization := "com.ephox"
    , version := "5.0-SNAPSHOT"
    , scalaVersion := "2.9.2"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT" withSources
      , "org.scalacheck" %% "scalacheck" % "1.9" % "test" withSources
      )
    , initialCommands := """
                           |import com.ephox.argonaut._
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
    , organization := "com.ephox"
    , version := "1.0"
    , scalaVersion := "2.9.2"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT" withSources
      , "org.scalacheck" %% "scalacheck" % "1.9" % "test" withSources
      )
    )
  )
}
