import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import Tools.onVersion
import sbtrelease.ReleasePlugin._
import com.typesafe.tools.mima.plugin.MimaPlugin._
import com.typesafe.tools.mima.plugin.MimaKeys._

object build extends Build {
  type Sett = Def.Setting[_]

  val base = ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion              = "7.2.0"
  val paradiseVersion            = "2.1.0-M5"
  val monocleVersion             = "1.3.0-SNAPSHOT"
  val catsVersion                = "0.4.0"
  val scalaz                     = "org.scalaz"                   %% "scalaz-core"               % scalazVersion
  val scalazScalaCheckBinding    = "org.scalaz"                   %% "scalaz-scalacheck-binding" % scalazVersion            % "test" exclude("org.scalacheck", "scalacheck_2.11") exclude("org.scalacheck", "scalacheck_2.10")
  val scalacheck                 = "org.scalacheck"               %% "scalacheck"                % "1.11.4"                 % "test"
  val specs2Scalacheck           = "org.specs2"                   %% "specs2-scalacheck"         % "3.6.6-scalaz-7.2.0"     % "test"
  val caliper                    = "com.google.caliper"           %  "caliper"                   % "0.5-rc1"
  val liftjson                   = "net.liftweb"                  %% "lift-json"                 % "2.6-RC1"
  val jackson                    = "com.fasterxml.jackson.core"   %  "jackson-core"              % "2.4.1.1"
  val monocle                    = "com.github.julien-truffaut"   %% "monocle-core"              % monocleVersion
  val monocleMacro               = "com.github.julien-truffaut"   %% "monocle-macro"             % monocleVersion
  val monocleLaw                 = "com.github.julien-truffaut"   %% "monocle-law"               % monocleVersion           % "test" exclude("org.scalacheck", "scalacheck_2.11") exclude("org.scalacheck", "scalacheck_2.10")
  val cats                       = "org.typelevel"                %% "cats"                      % catsVersion
  val catsLaw                    = "org.typelevel"                %% "cats-laws"                 % catsVersion              % "test" exclude("org.scalacheck", "scalacheck_2.11") exclude("org.scalacheck", "scalacheck_2.10")
  val catsTests                  = "org.typelevel"                %% "cats-tests"                % catsVersion              % "test" exclude("org.scalacheck", "scalacheck_2.11") exclude("org.scalacheck", "scalacheck_2.10")

  def reflect(v: String)         =
                                    Seq("org.scala-lang" % "scala-reflect"  % v) ++
           (if (v.contains("2.10")) Seq("org.scalamacros" %% "quasiquotes" % paradiseVersion) else Seq())

  val commonSettings = base ++
    ReplSettings.all ++
    releaseSettings ++
    PublishSettings.all ++
    InfoSettings.all ++
    Seq(addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)) ++
    net.virtualvoid.sbt.graph.Plugin.graphSettings ++ Seq[Sett](
      scalacOptions += "-language:_"
    , (sourceGenerators in Compile) <+= (sourceManaged in Compile) map Boilerplate.gen
    , resolvers += Resolver.sonatypeRepo("releases")
    , resolvers += Resolver.sonatypeRepo("snapshots")
    , resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
    , autoScalaLibrary := false
    , libraryDependencies ++= Seq(
      scalacheck
    , specs2Scalacheck
    ) ++ reflect(scalaVersion.value)
    // no mima until 6.1.0 release.
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

  val argonaut = Project(
    id = "argonaut"
  , base = file("argonaut")
  , settings = commonSettings ++ Seq[Sett](
      name := "argonaut"
    , libraryDependencies ++= Seq(
          scalacheck
        , specs2Scalacheck
      )
    )
  )

  val argonautScalaz = Project(
    id = "argonaut-scalaz"
  , base = file("argonaut-scalaz")
  , settings = commonSettings ++ Seq(
      name := "argonaut-scalaz"
    , libraryDependencies ++= Seq(
          scalaz
        , scalacheck
        , specs2Scalacheck
        , scalazScalaCheckBinding
      )
    )
  ).dependsOn(argonaut % "compile->compile;test->test")

  val argonautMonocle = Project(
    id = "argonaut-monocle"
  , base = file("argonaut-monocle")
  , settings = commonSettings ++ Seq[Sett](
      name := "argonaut-monocle"
    , libraryDependencies ++= Seq(
          monocle
        , monocleMacro
        , monocleLaw
      )
    )
  ).dependsOn(argonaut % "compile->compile;test->test", argonautScalaz % "compile->compile;test->test")

  val argonautCats = Project(
    id = "argonaut-cats"
    , base = file("argonaut-cats")
    , settings = commonSettings ++ Seq(
      name := "argonaut-cats"
      , libraryDependencies ++= Seq(
        cats
        , catsLaw
        , scalacheck
        , specs2Scalacheck
      )
    )
  ).dependsOn(argonaut % "compile->compile;test->test")

  val argonautBenchmark = Project(
    id = "argonaut-benchmark"
  , base = file("argonaut-benchmark")
  , settings = base ++ releaseSettings ++ PublishSettings.all ++ Seq[Sett](
      name := "argonaut-benchmark"
    , fork in run := true
    , publishArtifact := false
    , libraryDependencies ++= Seq(caliper, liftjson, jackson)
    , javaOptions in run <++= (fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")) }
    )
  ).dependsOn(argonaut)

  val argonautParent = Project(
    id = "argonaut-parent"
  , base = file(".")
  , settings = base ++ releaseSettings ++ PublishSettings.all ++ Seq[Sett](
      name := "argonaut-parent"
    , fork in run := true
    , publishArtifact := false
    )
  ).aggregate(argonaut, argonautScalaz, argonautMonocle, argonautCats, argonautBenchmark)
}
