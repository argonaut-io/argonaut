import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import Tools.onVersion
import sbtrelease.ReleasePlugin._
import com.typesafe.tools.mima.plugin.MimaPlugin._
import com.typesafe.tools.mima.plugin.MimaKeys._

object build extends Build {
  type Sett = Def.Setting[_]

  val base = Defaults.defaultSettings ++ ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion              = "7.1.0-M7"
  val scalaz                     = "org.scalaz"                   %% "scalaz-core"               % scalazVersion
  val scalazScalaCheckBinding    = "org.scalaz"                   %% "scalaz-scalacheck-binding" % scalazVersion            % "test"
  val caliper                    = "com.google.caliper"           %  "caliper"                   % "0.5-rc1"
  val liftjson                   = "net.liftweb"                  %  "lift-json_2.9.2"           % "2.5-M3"
  val jackson                    = "com.fasterxml.jackson.core"   % "jackson-core"               % "2.3.2"

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
        all = Seq(scalaz, scalazScalaCheckBinding)
      , on210 = Seq("org.specs2" % "specs2-scalacheck_2.10" % "2.3.12-scalaz-7.1.0-M6" % "test")
      , on211 = Seq("org.specs2" % "specs2-scalacheck_2.11" % "2.3.12-scalaz-7.1.0-M7" % "test")
      )
     /* no mima until 6.1.0 release */
    , previousArtifact := None
/*    , binaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      /* adding functions to sealed traits is binary incompatible from java, but ok for scala, so ignoring */
      Seq(
      ) map exclude[MissingMethodProblem] } */
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
