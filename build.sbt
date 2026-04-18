import build._
import ScalaSettings.scalaVersions

val argonaut = projectMatrix
  .in(file("argonaut"))
  .defaultAxes()
  .settings(
    build.commonSettings,
    InfoSettings.all,
    name := "argonaut",
    Compile / sourceGenerators += (Compile / sourceManaged).map(Boilerplate.gen).taskValue
  )
  .jvmPlatform(
    scalaVersions = scalaVersions,
    settings = jvmSettings,
  )
  .nativePlatform(
    scalaVersions = scalaVersions,
    settings = nativeSettings
  )
  .jsPlatform(
    scalaVersions = scalaVersions,
    settings = jsSettings
  )

val argonautScalaz = projectMatrix
  .in(file("argonaut-scalaz"))
  .defaultAxes()
  .settings(
    build.commonSettings,
    name := "argonaut-scalaz",
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core" % scalazVersion
    ),
    libraryDependencies += "org.scalaz" %%% "scalaz-scalacheck-binding" % scalazVersion % "test",
  )
  .jvmPlatform(
    scalaVersions = scalaVersions,
    settings = jvmSettings,
  )
  .nativePlatform(
    scalaVersions = scalaVersions,
    settings = nativeSettings
  )
  .jsPlatform(
    scalaVersions = scalaVersions,
    settings = jsSettings
  )
  .dependsOn(argonaut % "compile->compile;test->test")

val argonautMonocle = projectMatrix
  .in(file("argonaut-monocle"))
  .defaultAxes()
  .settings(
    build.commonSettings,
    name := "argonaut-monocle3",
    libraryDependencies ++= Seq(
      "dev.optics" %%% "monocle-core" % monocleVersion,
      "dev.optics" %%% "monocle-macro" % monocleVersion,
      "dev.optics" %%% "monocle-law" % monocleVersion % "test"
    )
  )
  .jvmPlatform(
    scalaVersions = scalaVersions.filterNot(ScalaSettings.Scala212 == _),
    settings = jvmSettings,
  )
  .nativePlatform(
    scalaVersions = scalaVersions.filterNot(ScalaSettings.Scala212 == _),
    settings = Def.settings(
      nativeSettings,
      mimaPreviousArtifacts := Set.empty
    )
  )
  .jsPlatform(
    scalaVersions = scalaVersions.filterNot(ScalaSettings.Scala212 == _),
    settings = jsSettings
  )
  .dependsOn(argonaut % "compile->compile;test->test", argonautCats % "compile->compile;test->test")

lazy val argonautCats = projectMatrix
  .in(file("argonaut-cats"))
  .defaultAxes()
  .settings(
    build.commonSettings,
    name := "argonaut-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "cats-laws" % catsVersion % "test"
    )
  )
  .jvmPlatform(
    scalaVersions = scalaVersions,
    settings = jvmSettings,
  )
  .nativePlatform(
    scalaVersions = scalaVersions,
    settings = nativeSettings
  )
  .jsPlatform(
    scalaVersions = scalaVersions,
    settings = jsSettings
  )
  .dependsOn(argonaut % "compile->compile;test->test")

val argonautJawn = projectMatrix
  .in(file("argonaut-jawn"))
  .defaultAxes()
  .settings(
    build.commonSettings,
    name := "argonaut-jawn",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "jawn-parser" % "1.6.0"
    )
  )
  .jvmPlatform(
    scalaVersions = scalaVersions,
    settings = jvmSettings,
  )
  .nativePlatform(
    scalaVersions = scalaVersions,
    settings = nativeSettings
  )
  .jsPlatform(
    scalaVersions = scalaVersions,
    settings = jsSettings
  )
  .dependsOn(argonaut % "compile->compile;test->test")

val argonautBenchmark = projectMatrix
  .defaultAxes()
  .settings(
    base,
    ReleasePlugin.projectSettings,
    PublishSettings.all,
    name := "argonaut-benchmark",
    run / fork := true,
    noPublish,
    libraryDependencies ++= Seq(
      "com.google.caliper" % "caliper" % "0.5-rc1",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.21.2"
    ),
    (run / javaOptions) ++= ((Runtime / fullClasspath) map { cp =>
      Seq("-cp", sbt.Attributed.data(cp).mkString(":"))
    }).value
  )
  .jvmPlatform(
    scalaVersions = Seq(ScalaSettings.Scala3),
    settings = jvmSettings ++ noPublish,
  )
  .dependsOn(argonaut)

lazy val noPublish = Seq(
  publish / skip := true,
  mimaFailOnNoPrevious := false,
  mimaPreviousArtifacts := Set.empty,
  PgpKeys.publishSigned := {},
  PgpKeys.publishLocalSigned := {},
  publishLocal := {},
  Compile / publishArtifact := false,
  publish := {}
)

base
ReleasePlugin.projectSettings
mimaFailOnNoPrevious := false
PublishSettings.all
noPublish
name := "argonaut-parent"
run / fork := true
