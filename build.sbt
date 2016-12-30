import Tools.onVersion
import build._
import sbtrelease.ReleasePlugin

val argonaut = argonautCrossProject("argonaut").settings(
  commonSettings ++ Seq[Sett](
    name := "argonaut"
  , (sourceGenerators in Compile) += ((sourceManaged in Compile) map Boilerplate.gen).taskValue
  )
)

val argonautJVM = argonaut.jvm
val argonautJS  = argonaut.js


val argonautScalaz = argonautCrossProject("argonaut-scalaz").settings(
  commonSettings ++ Seq(
    name := "argonaut-scalaz"
  , libraryDependencies ++= Seq(
      "org.scalaz"                   %%% "scalaz-core"               % scalazVersion
    , "org.scalaz"                   %%% "scalaz-scalacheck-binding" % s"${scalazVersion}-scalacheck-1.13" % "test"
    )
  )
).dependsOn(argonaut % "compile->compile;test->test")

val argonautScalazJVM = argonautScalaz.jvm
val argonautScalazJS  = argonautScalaz.js


val argonautMonocle = argonautCrossProject("argonaut-monocle").settings(
  commonSettings ++ Seq[Sett](
    name := "argonaut-monocle"
  , libraryDependencies ++= Seq(
      "com.github.julien-truffaut"   %%% "monocle-core"              % monocleVersion
    , "com.github.julien-truffaut"   %%% "monocle-macro"             % monocleVersion
    , "com.github.julien-truffaut"   %%% "monocle-law"               % monocleVersion           % "test"
    )
  )
).dependsOn(argonaut % "compile->compile;test->test", argonautScalaz % "compile->compile;test->test")

val argonautMonocleJVM = argonautMonocle.jvm
val argonautMonocleJS  = argonautMonocle.js


val argonautCats = argonautCrossProject("argonaut-cats").settings(
  commonSettings ++ Seq(
    name := "argonaut-cats"
  , libraryDependencies ++= Seq(
      "org.typelevel"                %%% "cats-core"                 % catsVersion
    , "org.typelevel"                %%% "cats-laws"                 % catsVersion              % "test"
    )
  )
).dependsOn(argonaut % "compile->compile;test->test")

val argonautCatsJVM = argonautCats.jvm
val argonautCatsJS  = argonautCats.js


val argonautJawn = Project(
  id = "argonaut-jawn"
, base = file("argonaut-jawn")
, settings = commonSettings ++ Seq[Sett](
    name := "argonaut-jawn"
  , libraryDependencies ++= Seq(
      "org.spire-math"               %%  "jawn-parser"               % "0.10.3"
    )
  )
).dependsOn(argonautJVM % "compile->compile;test->test")


val argonautBenchmark = Project(
  id = "argonaut-benchmark"
, base = file("argonaut-benchmark")
, settings = base ++ ReleasePlugin.projectSettings ++ PublishSettings.all ++ Seq[Sett](
    name := "argonaut-benchmark"
  , fork in run := true
  , publishArtifact := false
  , libraryDependencies ++= Seq(
      "com.google.caliper"           %   "caliper"                   % "0.5-rc1"
    , "com.fasterxml.jackson.core"   %   "jackson-core"              % "2.4.1.1"
    )
  , javaOptions in run ++= ((fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")) }).value
  )
).dependsOn(argonautJVM)


val jsProjects = Seq[ProjectReference](
  argonautJS, argonautScalazJS, argonautMonocleJS, argonautCatsJS
)

val jvmProjects = Seq[ProjectReference](
  argonautJVM, argonautScalazJVM, argonautMonocleJVM, argonautCatsJVM, argonautJawn, argonautBenchmark
)

val argonautParent = Project(
  id = "argonaut-parent"
, base = file(".")
, settings = base ++ ReleasePlugin.projectSettings ++ PublishSettings.all ++ Seq[Sett](
    name := "argonaut-parent"
  , fork in run := true
  , publishArtifact := false
  )
, aggregate = jsProjects ++ jvmProjects)
