import build._

val dottySetting = {
  libraryDependencies := {
    val previous = libraryDependencies.value

    // https://github.com/lampepfl/dotty/pull/9637/
    val jsExcludeNames = Set(
      "scala3-library_sjs1",
      "dotty-library_sjs1"
    )

    previous.map(x =>
      if (jsExcludeNames(x.name) && isDottyJS.value) {
        x
      } else {
        x.withDottyCompat(scalaVersion.value)
      }
    )
  }
}

val argonaut = argonautCrossProject(
    "argonaut"
  , Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  InfoSettings.all ++ Seq[Sett](
    name := "argonaut"
  , (Compile / sourceGenerators) += ((Compile / sourceManaged) map Boilerplate.gen).taskValue
  , dottySetting
  )
)

val argonautJVM = argonaut.jvm
val argonautJS  = argonaut.js
val argonautNative = argonaut.native


val argonautScalaz = argonautCrossProject(
    "argonaut-scalaz"
  , Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  Seq(
    name := "argonaut-scalaz"
  , libraryDependencies ++= Seq(
      "org.scalaz"                   %%% "scalaz-core"               % scalazVersion
    )
  )
).platformsSettings(JVMPlatform, JSPlatform)(
  libraryDependencies += "org.scalaz" %%% "scalaz-scalacheck-binding" % scalazVersion % "test",
  dottySetting
).dependsOn(argonaut % "compile->compile;test->test")

val argonautScalazJVM = argonautScalaz.jvm
val argonautScalazJS  = argonautScalaz.js
val argonautScalazNative = argonautScalaz.native


val argonautMonocle = argonautCrossProject(
    "argonaut-monocle"
  , Seq(JVMPlatform, JSPlatform)
).settings(
  Seq[Sett](
    name := "argonaut-monocle"
  , libraryDependencies ++= Seq(
      "com.github.julien-truffaut"   %%% "monocle-core"              % monocleVersion
    , "com.github.julien-truffaut"   %%% "monocle-macro"             % monocleVersion
    , "com.github.julien-truffaut"   %%% "monocle-law"               % monocleVersion % "test"
    )
  , dottySetting
  )
).dependsOn(argonaut % "compile->compile;test->test", argonautScalaz % "compile->compile;test->test")

val argonautMonocleJVM = argonautMonocle.jvm
val argonautMonocleJS  = argonautMonocle.js


val argonautCats = argonautCrossProject(
    "argonaut-cats"
  , Seq(JVMPlatform, JSPlatform)
).settings(
  Seq(
    name := "argonaut-cats"
  , libraryDependencies ++= Seq(
      "org.typelevel"                %%% "cats-core"                 % catsVersion
    , "org.typelevel"                %%% "cats-laws"                 % catsVersion              % "test"
    , "org.typelevel"                %%% "discipline-specs2"         % "1.1.4"                  % "test"
    )
  )
  , dottySetting
).dependsOn(argonaut % "compile->compile;test->test")

val argonautCatsJVM = argonautCats.jvm
val argonautCatsJS  = argonautCats.js


val argonautJawn = argonautCrossProject(
    "argonaut-jawn"
  , Seq(JVMPlatform)
).settings(
  Seq(
    name := "argonaut-jawn"
  , libraryDependencies ++= Seq(
      "org.typelevel"               %%%  "jawn-parser"               % "1.0.3"
    )
  , dottySetting
  )
).dependsOn(argonaut % "compile->compile;test->test")

val argonautJawnJVM = argonautJawn.jvm

val argonautBenchmark = Project(
  id = "argonaut-benchmark"
, base = file("argonaut-benchmark")
).settings(
  base ++ ReleasePlugin.projectSettings ++ PublishSettings.all ++ Seq[Sett](
    name := "argonaut-benchmark"
  , run / fork := true
  , publishArtifact := false
  , mimaFailOnNoPrevious := false
  , libraryDependencies ++= Seq(
      "com.google.caliper"           %   "caliper"                   % "0.5-rc1"
    , "com.fasterxml.jackson.core"   %   "jackson-core"              % "2.12.1"
    )
  , (run / javaOptions) ++= ((Runtime / fullClasspath) map { cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")) }).value
  )
).dependsOn(argonautJVM)

val nativeTest = Project(
  nativeTestId
, file("native-test")
)
.enablePlugins(ScalaNativePlugin)
.settings(
    base
  , noPublish
  , nativeSettings
).dependsOn(
  nativeProjects.map(p => p: ClasspathDep[ProjectReference]) : _*
)

lazy val nativeProjects = Seq[ProjectReference](
  argonautNative, argonautScalazNative
)

val jsProjects = Seq(
  argonautJS, argonautScalazJS, argonautMonocleJS, argonautCatsJS
)

val jvmProjects = Seq(
  argonautJVM, argonautScalazJVM, argonautMonocleJVM, argonautCatsJVM, argonautJawnJVM, argonautBenchmark
)

lazy val noPublish = Seq(
  PgpKeys.publishSigned := {},
  PgpKeys.publishLocalSigned := {},
  publishLocal := {},
  Compile / publishArtifact := false,
  publish := {}
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

val jvmParent = project
  .settings(
    base
  , noPublish
  ).aggregate(
    jvmProjects.map(p => p: ProjectReference) : _*
  )

val jsParent = project
  .settings(
    base
  , noPublish
  , commands += Command.command("testSequential"){
      // avoid "org.scalajs.jsenv.ComJSEnv$ComClosedException: Node.js isn't connected" error in CI
      jsProjects.map(_.id + "/test").sorted.toList ::: _
    }
  , commands += Command.command("testSequentialCross"){
      jsProjects.map(x => s"+${x.id}/test").sorted.toList ::: _
    }
  ).aggregate(
    jsProjects.map(p => p: ProjectReference) : _*
  )

val argonautParent = Project(
  id = "argonaut-parent"
, base = file(".")
).settings(
    base
  , ReleasePlugin.projectSettings
  , mimaFailOnNoPrevious := false
  , PublishSettings.all
  , noPublish
  , name := "argonaut-parent"
  , run / fork := true
).aggregate(
  (jsProjects ++ jvmProjects).map(p => p: ProjectReference) : _*
)
