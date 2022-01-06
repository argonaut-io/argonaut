import build._

lazy val disableScala2_12 = Def.settings(
  mimaPreviousArtifacts := {
    if (scalaBinaryVersion.value == "2.12") {
      Set.empty
    } else {
      mimaPreviousArtifacts.value
    }
  },
  libraryDependencies := {
    if (scalaBinaryVersion.value == "2.12") {
      Nil
    } else {
      libraryDependencies.value
    }
  },
  Seq(Compile, Test).map { x =>
    (x / sources) := {
      if (scalaBinaryVersion.value == "2.12") {
        Nil
      } else {
        (x / sources).value
      }
    }
  },
  Test / test := {
    if (scalaBinaryVersion.value == "2.12") {
      ()
    } else {
      (Test / test).value
    }
  },
  publish / skip := (scalaBinaryVersion.value == "2.12")
)

val argonaut = argonautCrossProject(
    "argonaut"
  , Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  InfoSettings.all ++ Seq[Sett](
    name := "argonaut"
  , (Compile / sourceGenerators) += ((Compile / sourceManaged) map Boilerplate.gen).taskValue
  )
)

val argonautJVM = argonaut.jvm
val argonautJS  = argonaut.js
val argonautNative = argonaut.native

lazy val conflictWarningSetting = Def.settings(
  conflictWarning := {
    if (scalaBinaryVersion.value == "3") {
      // TODO
      ConflictWarning("warn", Level.Warn, false)
    } else {
      conflictWarning.value
    }
  }
)

val argonautScalaz = argonautCrossProject(
    "argonaut-scalaz"
  , Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
    name := "argonaut-scalaz"
  , libraryDependencies ++= Seq(
      "org.scalaz"                   %%% "scalaz-core"               % scalazVersion cross CrossVersion.for3Use2_13
    )
  , conflictWarningSetting
).platformsSettings(JVMPlatform, JSPlatform)(
  libraryDependencies += "org.scalaz" %%% "scalaz-scalacheck-binding" % scalazVersion % "test" cross CrossVersion.for3Use2_13,
).dependsOn(argonaut % "compile->compile;test->test")

val argonautScalazJVM = argonautScalaz.jvm
val argonautScalazJS  = argonautScalaz.js
val argonautScalazNative = argonautScalaz.native


val argonautMonocle = argonautCrossProject(
    "argonaut-monocle"
  , Seq(JVMPlatform, JSPlatform)
).settings(
    name := "argonaut-monocle3"
  , conflictWarningSetting
  , previousVersions --= (0 to 6).map(n => s"6.3.$n")
  , libraryDependencies ++= Seq(
      "dev.optics"   %%% "monocle-core"              % monocleVersion
    , "dev.optics"   %%% "monocle-macro"             % monocleVersion
    , "dev.optics"   %%% "monocle-law"               % monocleVersion % "test"
    )
  , disableScala2_12
).dependsOn(argonaut % "compile->compile;test->test", argonautCats % "compile->compile;test->test")

val argonautMonocleJVM = argonautMonocle.jvm
val argonautMonocleJS  = argonautMonocle.js


lazy val argonautCats = argonautCrossProject(
    "argonaut-cats"
  , Seq(JVMPlatform, JSPlatform)
).settings(
    name := "argonaut-cats"
  , libraryDependencies ++= Seq(
      "org.typelevel"                %%% "cats-core"                 % catsVersion
    , "org.typelevel"                %%% "cats-laws"                 % catsVersion              % "test"
    )
).dependsOn(argonaut % "compile->compile;test->test")

val argonautCatsJVM = argonautCats.jvm
val argonautCatsJS  = argonautCats.js


val argonautJawn = argonautCrossProject(
    "argonaut-jawn"
  , Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
    name := "argonaut-jawn"
  , libraryDependencies ++= Seq(
      "org.typelevel"               %%%  "jawn-parser"               % "1.3.2"
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
    , "com.fasterxml.jackson.core"   %   "jackson-core"              % "2.13.1"
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
  argonautNative, argonautScalazNative, argonautJawnNative
)

val jsProjects = Seq(
  argonautJS, argonautScalazJS, argonautMonocleJS, argonautCatsJS, argonautJawnJS
)

val jvmProjects = Seq(
  argonautJVM, argonautScalazJVM, argonautMonocleJVM, argonautCatsJVM, argonautJawnJVM, argonautBenchmark
)

lazy val noPublish = Seq(
  PgpKeys.publishSigned := {},
  PgpKeys.publishLocalSigned := {},
  publishLocal := {},
  previousVersions := Nil,
  Compile / publishArtifact := false,
  publish := {}
)

val nativeParent = Project(
  nativeParentId
, file("native-parent")
).settings(
    base
  , noPublish
  , nativeSettings
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
