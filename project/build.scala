import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import Tools.onVersion
import sbtrelease.ReleasePlugin._

object build extends Build {
  type Sett = Project.Setting[_]

  val base = Defaults.defaultSettings ++ ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.0-SNAPSHOT"
  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
  val scalazScalaCheckBinding = "org.scalaz" %% "scalaz-scalacheck-binding" % "7.1.0-SNAPSHOT" % "test"
  val specs2 = "org.specs2" %% "specs2" % "2.2-scalaz-7.1.0-SNAPSHOT" % "test"
  val caliper = "com.google.caliper" % "caliper" % "0.5-rc1"
  val liftjson = "net.liftweb" % "lift-json_2.9.2" % "2.5-M3"
  val jackson = "com.fasterxml.jackson.core" % "jackson-core" % "2.1.1"


  val argonaut = Project(
    id = "argonaut"
  , base = file(".")
  , settings = base ++ ReplSettings.all ++ releaseSettings ++ PublishSettings.all ++ InfoSettings.all ++ Seq[Sett](
      name := "argonaut"
    , (sourceGenerators in Compile) <+= (sourceManaged in Compile) map Boilerplate.gen
    , resolvers ++= Seq(
        "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"
      , "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
      )
    , libraryDependencies <++= onVersion(
        all = Seq(scalaz, scalacheck, scalazScalaCheckBinding, specs2)
      )
    )
  )

  val benchmark = Project(
    id = "benchmark"
  , base = file("benchmark")
  , dependencies = Seq(argonaut)
  , settings = base ++ Seq[Sett](
      name := "argonaut-benchmark"
    , fork in run := true
    , libraryDependencies ++= Seq(caliper, liftjson, jackson)
    , javaOptions in run <++= (fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Build.data(cp).mkString(":")) }
    )
  )

  val doc = Project(
    id = "doc"
  , base = file("doc")
  , dependencies = Seq(argonaut)
  , settings = base ++ Seq[Sett](
      name := "argonaut-doc"
    )
  )
}
