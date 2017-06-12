import build._
import sbtrelease.ReleasePlugin
import sbtcrossproject.CrossProject.crossClasspathDependencyConstructor

val argonaut = argonautCrossProject(
    "argonaut"
  , Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  commonSettings ++ InfoSettings.all ++ Seq[Sett](
    name := "argonaut"
  , (sourceGenerators in Compile) += ((sourceManaged in Compile) map Boilerplate.gen).taskValue
  )
)

val argonautJVM = argonaut.jvm
val argonautJS  = argonaut.js
val argonautNative = argonaut.native


val argonautScalaz = argonautCrossProject(
    "argonaut-scalaz"
  , Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  commonSettings ++ Seq(
    name := "argonaut-scalaz"
  , libraryDependencies ++= Seq(
      "org.scalaz"                   %%% "scalaz-core"               % scalazVersion
    )
  )
).platformsSettings(JVMPlatform, JSPlatform)(
  libraryDependencies ++= Seq(
    "org.scalaz"                     %%% "scalaz-scalacheck-binding" % s"${scalazVersion}-scalacheck-1.13" % "test"
  )
).dependsOn(argonaut % "compile->compile;test->test")

val argonautScalazJVM = argonautScalaz.jvm
val argonautScalazJS  = argonautScalaz.js
val argonautScalazNative = argonautScalaz.native


val argonautMonocle = argonautCrossProject(
    "argonaut-monocle"
  , Seq(JVMPlatform, JSPlatform)
).settings(
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


val argonautCats = argonautCrossProject(
    "argonaut-cats"
  , Seq(JVMPlatform, JSPlatform)
).settings(
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
      "org.spire-math"               %%  "jawn-parser"               % "0.10.4"
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
    , "com.fasterxml.jackson.core"   %   "jackson-core"              % "2.8.7"
    )
  , javaOptions in run ++= ((fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")) }).value
  )
).dependsOn(argonautJVM)


val nativeProjects = Seq[ProjectReference](
  argonautNative, argonautScalazNative
)

val jsProjects = Seq[ProjectReference](
  argonautJS, argonautScalazJS, argonautMonocleJS, argonautCatsJS
)

val jvmProjects = Seq[ProjectReference](
  argonautJVM, argonautScalazJVM, argonautMonocleJVM, argonautCatsJVM, argonautJawn, argonautBenchmark
)

lazy val noPublish = Seq(
  PgpKeys.publishSigned := {},
  PgpKeys.publishLocalSigned := {},
  publishLocal := {},
  publishArtifact in Compile := false,
  publish := {}
)

val nativeTest = Project(
  nativeTestId
, file("native-test")
).enablePlugins(ScalaNativePlugin).settings(
    base
  , noPublish
  , nativeSettings
).dependsOn(
  nativeProjects.map(p => p: ClasspathDep[ProjectReference]) : _*
)

val nativeParent = Project(
  nativeParentId
, file("native-parent")
).settings(
    base
  , noPublish
).aggregate(
  nativeProjects : _*
)

val argonautParent = Project(
  id = "argonaut-parent"
, base = file(".")
).settings(
    base
  , ReleasePlugin.projectSettings
  , PublishSettings.all
  , noPublish
  , name := "argonaut-parent"
  , fork in run := true
).aggregate(
  jsProjects ++ jvmProjects : _*
)
