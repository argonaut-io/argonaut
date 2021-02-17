import sbt._
import Keys._
import com.jsuereth.sbtpgp.PgpKeys._
import sbtrelease.ReleasePlugin.autoImport._
import xerial.sbt.Sonatype.autoImport._

object PublishSettings {
  type Sett = Def.Setting[_]

  def Scala3 = "3.0.0-RC1"

  // TODO monocle
  val scala3projects = Seq(
    "argonautJVM",
    "argonautJS",
    "argonaut-catsJVM",
    "argonaut-jawnJVM",
    "argonaut-scalazJVM",
    "argonaut-catsJS",
    "argonaut-scalazJS",
  )

  lazy val all = Seq[Sett](
    pom
  , publish
  , publishMavenStyle := true
  , publishArtifact in Test := false
  , pomIncludeRepository := { _ => false }
  , releasePublishArtifactsAction := publishSigned.value
  , releaseProcess := {
     import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._
     Seq[ReleaseStep](
       checkSnapshotDependencies,
       inquireVersions,
       runTest,
       releaseStepCommandAndRemaining("+ " + build.nativeTestId + "/run"),
       setReleaseVersion,
       commitReleaseVersion,
       tagRelease,
       publishArtifacts,
       releaseStepCommandAndRemaining(s"++ ${Scala3}!;all ${scala3projects.map(_ + "/publishSigned").mkString(" ")}"),
       releaseStepCommandAndRemaining("+ " + build.nativeParentId + "/publishSigned"),
       releaseStepCommandAndRemaining("sonatypeBundleRelease"),
       setNextVersion,
       commitNextVersion,
       pushChanges
    )}
  , releaseCrossBuild := true
  , licenses := Seq("BSD-3-Clause" -> url("http://www.opensource.org/licenses/BSD-3-Clause"))
  , homepage := Some(url("http://argonaut.io"))
  , autoAPIMappings := true
  , apiURL := Some(url("http://argonaut.io/scaladocs/"))
  , useGpg := true
  )

  lazy val pom: Sett =
    pomExtra := (
      <scm>
        <url>git@github.com:argonaut-io/argonaut.git</url>
        <connection>scm:git:git@github.com:argonaut-io/argonaut.git</connection>
      </scm>
      <developers>
        <developer>
          <id>tonymorris</id>
          <name>Tony Morris</name>
          <url>http://tmorris.net</url>
        </developer>
        <developer>
          <id>mth</id>
          <name>Mark Hibberd</name>
          <url>http://mth.io</url>
        </developer>
        <developer>
          <id>seanparsons</id>
          <name>Sean Parsons</name>
        </developer>
      </developers>
    )

  lazy val publish: Sett =
    publishTo := sonatypePublishToBundle.value
}
