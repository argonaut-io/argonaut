import build._

val disableScala3 = disableScala("3")

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
    "argonaut"
  , Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  commonSettings ++ InfoSettings.all ++ Seq[Sett](
    name := "argonaut"
  , (Compile / sourceGenerators) += ((Compile / sourceManaged) map Boilerplate.gen).taskValue
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
      "com.github.julien-truffaut"   %%% "monocle-core"              % monocleVersion.value
    , "com.github.julien-truffaut"   %%% "monocle-macro"             % monocleVersion.value
    , "com.github.julien-truffaut"   %%% "monocle-law"               % monocleVersion.value     % "test"
    )
  )
  , disableScala3
).dependsOn(argonaut % "compile->compile;test->test", argonautScalaz % "compile->compile;test->test")

val argonautMonocleJVM = argonautMonocle.jvm
val argonautMonocleJS  = argonautMonocle.js

val argonautCats = argonautCrossProject(
    "argonaut-cats"
  , Seq(JVMPlatform, JSPlatform, NativePlatform)
).settings(
  commonSettings,
  name := "argonaut-cats",
  libraryDependencies ++= Seq(
    "org.typelevel"                %%% "cats-core"                 % catsVersion.value
  , "org.typelevel"                %%% "cats-laws"                 % catsVersion.value        % "test"
  ),
).dependsOn(argonaut % "compile->compile;test->test")

val argonautCatsJVM = argonautCats.jvm
val argonautCatsJS  = argonautCats.js
val argonautCatsNative = argonautCats.native.settings(
  disableScala3,
)


val argonautJawn = argonautCrossProject(
    "argonaut-jawn"
  , Seq(JVMPlatform)
).settings(
  commonSettings ++ Seq(
    name := "argonaut-jawn"
  , libraryDependencies ++= Seq(
      "org.typelevel"               %%%  "jawn-parser"               % "0.14.3"
    )
  )
  , disableScala3
).dependsOn(argonaut % "compile->compile;test->test")

val argonautJawnJVM = argonautJawn.jvm

val argonautBenchmark = Project(
  id = "argonaut-benchmark"
, base = file("argonaut-benchmark")
).settings(
  base ++ ReleasePlugin.projectSettings ++ PublishSettings.all ++ Seq[Sett](
    name := "argonaut-benchmark"
  , (run / fork) := true
  , publishArtifact := false
  , libraryDependencies ++= Seq(
      "com.google.caliper"           %   "caliper"                   % "0.5-rc1"
    , "com.fasterxml.jackson.core"   %   "jackson-core"              % "2.17.1"
    )
  , (run / javaOptions) ++= ((Runtime / fullClasspath) map { cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")) }).value
  )
).dependsOn(argonautJVM)


val nativeProjects = Seq[ProjectReference](
  argonautNative, argonautScalazNative, argonautCatsNative
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
  (Compile / publishArtifact) := false,
  publish := {}
)

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
      // avoid "org.scalajs.jsenv.ComJSEnv$ComClosedException: Node.js isn't connected" error in travis-ci
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
  , PublishSettings.all
  , noPublish
  , name := "argonaut-parent"
  , (run / fork) := true
).aggregate(
  (jsProjects ++ jvmProjects).map(p => p: ProjectReference) : _*
)

(ThisBuild / mimaFailOnNoPrevious) := false
