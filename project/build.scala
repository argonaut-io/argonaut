import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import Tools.onVersion
import sbtrelease.ReleasePlugin._

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact
import com.typesafe.tools.mima.plugin.MimaKeys.binaryIssueFilters

object build extends Build {
  type Sett = Def.Setting[_]

  val base = Defaults.defaultSettings ++ ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion = "7.0.6"

  val scalaz = "org.scalaz" %% "scalaz-core" % scalazVersion
  val scalazScalaCheckBinding = "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
  val specs2_1_12_4_1 = "org.specs2" %% "specs2" % "1.12.4.1" % "test"
  val specs2_1_14 = "org.specs2" %% "specs2" % "1.14" % "test"
  val specs2_2_3 = "org.specs2" %% "specs2" % "2.3.10"
  val caliper = "com.google.caliper" % "caliper" % "0.5-rc1"
  val liftjson = "net.liftweb" % "lift-json_2.9.2" % "2.5-M3"
  val jackson = "com.fasterxml.jackson.core" % "jackson-core" % "2.1.1"

  val argonaut = Project(
    id = "argonaut"
  , base = file(".")
  , settings = base ++ ReplSettings.all ++ releaseSettings ++ PublishSettings.all ++ InfoSettings.all ++ mimaDefaultSettings ++ Seq[Sett](
      name := "argonaut"
    , (sourceGenerators in Compile) <+= (sourceManaged in Compile) map Boilerplate.gen
    , libraryDependencies <++= onVersion(
        all = Seq(scalaz, scalazScalaCheckBinding)
      , on292 = Seq(specs2_1_12_4_1)
      , on210 = Seq(specs2_1_14)
      , on211 = Seq(specs2_2_3)
      )
    , previousArtifact := {
        if(scalaVersion.value.startsWith("2.11"))
          None
        else
          Some(organization.value % (name.value + "_" + scalaBinaryVersion.value) % "6.0.1")
      }
    , binaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      Seq(
        "argonaut.CursorOp.fold"
      ) map exclude[MissingMethodProblem] }
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
    , javaOptions in run <++= (fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")) }
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
