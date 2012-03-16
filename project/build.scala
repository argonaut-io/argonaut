import sbt._
import Keys._

object build extends Build {
  type Sett = Project.Setting[_]

  val argonaut = Project(
    id = "argonaut"
  , base = file(".")
  , settings = Defaults.defaultSettings ++ Seq[Sett](
      name := "argonaut"
    , resolvers += "sonatype-snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    , organization := "com.ephox"
    , version := "1.0"
    , scalaVersion := "2.9.1"
    , scalacOptions := Seq(
        "-deprecation"
      , "-unchecked"
      )
    , libraryDependencies ++= Seq(
        "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test" withSources
      , "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT" withSources
      )
    )
  )
}
