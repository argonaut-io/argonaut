import build._

def disableScala2_12 = disableScala("2.12")

def disableScala(v: String) = Def.settings(
  mimaPreviousArtifacts := {
    if (scalaBinaryVersion.value == v) {
      Set.empty
    } else {
      mimaPreviousArtifacts.value
    }
  },
  libraryDependencies := {
    if (scalaBinaryVersion.value == v) {
      Nil
    } else {
      libraryDependencies.value
    }
  },
  Seq(Compile, Test).map { x =>
    (x / sources) := {
      if (scalaBinaryVersion.value == v) {
        Nil
      } else {
        (x / sources).value
      }
    }
  },
  Test / test := {
    if (scalaBinaryVersion.value == v) {
      ()
    } else {
      (Test / test).value
    }
  },
  publish / skip := (scalaBinaryVersion.value == v)
)

val argonaut = argonautCrossProject(
  "argonaut",
  Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  InfoSettings.all ++ Seq[Sett](
    name := "argonaut",
    (Compile / sourceGenerators) += ((Compile / sourceManaged) map Boilerplate.gen).taskValue
  )
)

val argonautJVM = argonaut.jvm
val argonautJS = argonaut.js
val argonautNative = argonaut.native

val argonautScalaz = argonautCrossProject(
  "argonaut-scalaz",
  Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  name := "argonaut-scalaz",
  libraryDependencies ++= Seq(
    "org.scalaz" %%% "scalaz-core" % scalazVersion
  )
).platformsSettings(JVMPlatform, JSPlatform, NativePlatform)(
  libraryDependencies += "org.scalaz" %%% "scalaz-scalacheck-binding" % scalazVersion % "test",
).dependsOn(argonaut % "compile->compile;test->test")

val argonautScalazJVM = argonautScalaz.jvm
val argonautScalazJS = argonautScalaz.js
val argonautScalazNative = argonautScalaz.native

val argonautMonocle = argonautCrossProject(
  "argonaut-monocle",
  Seq(JVMPlatform, JSPlatform)
).settings(
  name := "argonaut-monocle3",
  previousVersions --= (0 to 6).map(n => s"6.3.$n"),
  libraryDependencies ++= Seq(
    "dev.optics" %%% "monocle-core" % monocleVersion,
    "dev.optics" %%% "monocle-macro" % monocleVersion,
    "dev.optics" %%% "monocle-law" % monocleVersion % "test"
  ),
  disableScala2_12
).dependsOn(argonaut % "compile->compile;test->test", argonautCats % "compile->compile;test->test")

val argonautMonocleJVM = argonautMonocle.jvm
val argonautMonocleJS = argonautMonocle.js

lazy val argonautCats = argonautCrossProject(
  "argonaut-cats",
  Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  name := "argonaut-cats",
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % catsVersion,
    "org.typelevel" %%% "cats-laws" % catsVersion % "test"
  )
).dependsOn(argonaut % "compile->compile;test->test")

val argonautCatsJVM = argonautCats.jvm
val argonautCatsJS = argonautCats.js
val argonautCatsNative = argonautCats.native

val argonautJawn = argonautCrossProject(
  "argonaut-jawn",
  Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  name := "argonaut-jawn",
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "jawn-parser" % "1.6.0"
  )
).dependsOn(argonaut % "compile->compile;test->test")

val argonautJawnJVM = argonautJawn.jvm
val argonautJawnJS = argonautJawn.js.settings(
  mimaPreviousArtifacts := {
    mimaPreviousArtifacts.value -- (0 to 5).map { n =>
      organization.value %% s"${Keys.name.value}_sjs1" % s"6.3.${n}"
    }
  }
)
val argonautJawnNative = argonautJawn.native

val argonautBenchmark = Project(
  id = "argonaut-benchmark",
  base = file("argonaut-benchmark")
).settings(
  base ++ ReleasePlugin.projectSettings ++ PublishSettings.all ++ Seq[Sett](
    name := "argonaut-benchmark",
    run / fork := true,
    publishArtifact := false,
    mimaFailOnNoPrevious := false,
    libraryDependencies ++= Seq(
      "com.google.caliper" % "caliper" % "0.5-rc1",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.17.1"
    ),
    (run / javaOptions) ++= ((Runtime / fullClasspath) map { cp =>
      Seq("-cp", sbt.Attributed.data(cp).mkString(":"))
    }).value
  )
).dependsOn(argonautJVM)

lazy val nativeProjects = Seq[ProjectReference](
  argonautNative,
  argonautScalazNative,
  argonautJawnNative,
  argonautCatsNative
)

val jsProjects = Seq(
  argonautJS,
  argonautScalazJS,
  argonautMonocleJS,
  argonautCatsJS,
  argonautJawnJS
)

val jvmProjects = Seq(
  argonautJVM,
  argonautScalazJVM,
  argonautMonocleJVM,
  argonautCatsJVM,
  argonautJawnJVM,
  argonautBenchmark
)

lazy val noPublish = Seq(
  mimaFailOnNoPrevious := false,
  PgpKeys.publishSigned := {},
  PgpKeys.publishLocalSigned := {},
  publishLocal := {},
  previousVersions := Nil,
  Compile / publishArtifact := false,
  publish := {}
)

val nativeParent = Project(
  "nativeParent",
  file("native-parent")
).settings(
  base,
  noPublish,
  nativeSettings
).aggregate(
  nativeProjects *
)

val jvmParent = project
  .settings(
    base,
    noPublish
  )
  .aggregate(
    jvmProjects.map(p => p: ProjectReference) *
  )

val jsParent = project
  .settings(
    base,
    noPublish,
    commands += Command.command("testSequential") {
      // avoid "org.scalajs.jsenv.ComJSEnv$ComClosedException: Node.js isn't connected" error in CI
      jsProjects.map(_.id + "/test").sorted.toList ::: _
    },
    commands += Command.command("testSequentialCross") {
      jsProjects.map(x => s"+${x.id}/test").sorted.toList ::: _
    }
  )
  .aggregate(
    jsProjects.map(p => p: ProjectReference) *
  )

base
ReleasePlugin.projectSettings
mimaFailOnNoPrevious := false
PublishSettings.all
noPublish
name := "argonaut-parent"
run / fork := true
