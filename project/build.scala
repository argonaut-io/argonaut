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

  val scalazVersion              = "7.2.2"
  val paradiseVersion            = "2.0.1"
  val monocleVersion             = "1.2.1"
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

  def reflect(v: String)         =
                                    Seq("org.scala-lang" % "scala-reflect"  % v) ++
           (if (v.contains("2.10")) Seq("org.scalamacros" %% "quasiquotes" % paradiseVersion) else Seq())

  val argonaut = Project(
    id = "argonaut"
  , base = file(".")
  , settings = base ++
    ReplSettings.all ++
    releaseSettings ++
    PublishSettings.all ++
    InfoSettings.all ++
    mimaDefaultSettings ++
    Seq(addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)) ++
    net.virtualvoid.sbt.graph.Plugin.graphSettings ++ Seq[Sett](
      name := "argonaut"
    , (sourceGenerators in Compile) <+= (sourceManaged in Compile) map Boilerplate.gen
    , resolvers += Resolver.sonatypeRepo("releases")
    , resolvers += Resolver.sonatypeRepo("snapshots")
    , resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
    , autoScalaLibrary := false
    , libraryDependencies ++= Seq(
        scalaz
      , scalacheck
      , specs2Scalacheck
      , scalazScalaCheckBinding
      , monocle
      , monocleMacro
      , monocleLaw
      ) ++ reflect(scalaVersion.value)
    , previousArtifact := Some(organization.value % s"${name.value}_${scalaBinaryVersion.value}" % "6.1")
    , binaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      /* adding functions to sealed traits is binary incompatible from java, but ok for scala, so ignoring */
      Seq(
      ) map exclude[MissingMethodProblem] }
    )
  )

  val benchmark = Project(
    id = "benchmark"
  , base = file("benchmark")
  , settings = base ++ Seq[Sett](
      name := "argonaut-benchmark"
    , resolvers += Resolver.sonatypeRepo("releases")
    , resolvers += Resolver.sonatypeRepo("snapshots")
    , fork in run := true
    , libraryDependencies ++= Seq(caliper, liftjson, jackson)
    , javaOptions in run <++= (fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")) }
    , scalacOptions += "-language:_"
    )
  ) dependsOn (argonaut)

  val doc = Project(
    id = "doc"
  , base = file("doc")
  , dependencies = Seq(argonaut)
  , settings = base ++ Seq[Sett](
      name := "argonaut-doc"
    )
  )
}
