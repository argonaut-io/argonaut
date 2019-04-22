import build._

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
  libraryDependencies += "org.scalaz" %%% "scalaz-scalacheck-binding" % s"${scalazVersion}-scalacheck-1.14" % "test"
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


val argonautJawn = argonautCrossProject(
    "argonaut-jawn"
  , Seq(JVMPlatform)
).settings(
  commonSettings ++ Seq(
    name := "argonaut-jawn"
  , libraryDependencies ++= Seq(
      "org.typelevel"               %%%  "jawn-parser"               % "0.14.1"
    )
  )
).dependsOn(argonaut % "compile->compile;test->test")

val argonautJawnJVM = argonautJawn.jvm

val argonautBenchmark = Project(
  id = "argonaut-benchmark"
, base = file("argonaut-benchmark")
, settings = base ++ ReleasePlugin.projectSettings ++ PublishSettings.all ++ Seq[Sett](
    name := "argonaut-benchmark"
  , fork in run := true
  , publishArtifact := false
  , libraryDependencies ++= Seq(
      "com.google.caliper"           %   "caliper"                   % "0.5-rc1"
    , "com.fasterxml.jackson.core"   %   "jackson-core"              % "2.9.8"
    )
  , javaOptions in run ++= ((fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Attributed.data(cp).mkString(":")) }).value
  )
).dependsOn(argonautJVM)


val nativeProjects = Seq[ProjectReference](
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
  publishArtifact in Compile := false,
  publish := {}
)

val nativeTest = Project(
  nativeTestId
, file("native-test")
)
.enablePlugins(ScalaNativePlugin)
.disablePlugins(sbt.plugins.BackgroundRunPlugin)
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
      jsProjects.map(_.id + "/test").sorted ::: _
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
  , fork in run := true
).aggregate(
  (jsProjects ++ jvmProjects).map(p => p: ProjectReference) : _*
)
