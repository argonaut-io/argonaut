import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import Tools.onVersion
import sbtrelease.ReleasePlugin
import com.typesafe.tools.mima.plugin.MimaPlugin._
import com.typesafe.tools.mima.plugin.MimaKeys._

object build {
  type Sett = Def.Setting[_]

  val base = ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion              = "7.2.8"
  val paradiseVersion            = "2.1.0"
  val monocleVersion             = "1.4.0-M1"
  val catsVersion                = "0.8.1"
  val scalaz                     = "org.scalaz"                   %% "scalaz-core"               % scalazVersion
  val scalazScalaCheckBinding    = "org.scalaz"                   %% "scalaz-scalacheck-binding" % s"${scalazVersion}-scalacheck-1.13" % "test"
  val scalacheck                 = "org.scalacheck"               %% "scalacheck"                % "1.13.4"                 % "test"
  val specs2Scalacheck           = "org.specs2"                   %% "specs2-scalacheck"         % "3.8.6"                  % "test"
  val caliper                    = "com.google.caliper"           %  "caliper"                   % "0.5-rc1"
  val jackson                    = "com.fasterxml.jackson.core"   %  "jackson-core"              % "2.4.1.1"
  val jawnParser                 = "org.spire-math"               %% "jawn-parser"               % "0.10.3"
  val monocle                    = "com.github.julien-truffaut"   %% "monocle-core"              % monocleVersion
  val monocleMacro               = "com.github.julien-truffaut"   %% "monocle-macro"             % monocleVersion
  val monocleLaw                 = "com.github.julien-truffaut"   %% "monocle-law"               % monocleVersion           % "test"
  val cats                       = "org.typelevel"                %% "cats-core"                 % catsVersion
  val catsLaw                    = "org.typelevel"                %% "cats-laws"                 % catsVersion              % "test"
  val catsTests                  = "org.typelevel"                %% "cats-tests"                % catsVersion              % "test"

  def reflect(v: String)         =
                                    Seq("org.scala-lang" % "scala-reflect"  % v) ++
           (if (v.contains("2.10")) Seq("org.scalamacros" %% "quasiquotes" % paradiseVersion) else Seq())

  val commonSettings = base ++
    ReplSettings.all ++
    ReleasePlugin.projectSettings ++
    PublishSettings.all ++
    InfoSettings.all ++
    Seq(addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)) ++
    Seq[Sett](
      scalacOptions += "-language:_"
    , resolvers += Resolver.sonatypeRepo("releases")
    , resolvers += Resolver.sonatypeRepo("snapshots")
    , autoScalaLibrary := false
    , libraryDependencies ++= Seq(
      scalacheck
    , specs2Scalacheck
    ) ++ reflect(scalaVersion.value)
    // no mima until 6.2.0 release.
    , previousArtifact := None
    /*
    , binaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      /* adding functions to sealed traits is binary incompatible from java, but ok for scala, so ignoring */
      Seq(
      ) map exclude[MissingMethodProblem]
    }
    */
  )
}
