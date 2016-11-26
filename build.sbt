import Tools.onVersion
import build._
import sbtrelease.ReleasePlugin

val argonaut = Project(
  id = "argonaut"
, base = file("argonaut")
, settings = commonSettings ++ Seq[Sett](
    name := "argonaut"
  , (sourceGenerators in Compile) += ((sourceManaged in Compile) map Boilerplate.gen).taskValue
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
  , scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11)) =>
          // https://github.com/scala/make-release-notes/blob/9cfbdc8c92f94/experimental-backend.md#emitting-java-8-style-lambdas
          Seq(
            "-Ybackend:GenBCode",
            "-Ydelambdafy:method",
            "-target:jvm-1.8"
          )
        case _ =>
          Nil
      }
    }
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

val argonautJawn = Project(
  id = "argonaut-jawn"
, base = file("argonaut-jawn")
, settings = commonSettings ++ Seq[Sett](
    name := "argonaut-jawn"
  , libraryDependencies ++= Seq(
      jawnParser
    )
  )
).dependsOn(argonaut % "compile->compile;test->test")

val argonautBenchmark = Project(
  id = "argonaut-benchmark"
, base = file("argonaut-benchmark")
, settings = base ++ ReleasePlugin.projectSettings ++ PublishSettings.all ++ Seq[Sett](
    name := "argonaut-benchmark"
  , fork in run := true
  , publishArtifact := false
  , libraryDependencies ++= Seq(caliper, jackson)
  , javaOptions in run ++= ((fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")) }).value
  )
).dependsOn(argonaut)

val argonautParent = Project(
  id = "argonaut-parent"
, base = file(".")
, settings = base ++ ReleasePlugin.projectSettings ++ PublishSettings.all ++ Seq[Sett](
    name := "argonaut-parent"
  , fork in run := true
  , publishArtifact := false
  )
).aggregate(
  argonaut
, argonautScalaz
, argonautMonocle
, argonautCats
, argonautJawn
, argonautBenchmark)
