import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import sbtrelease.ReleasePlugin.autoImport._

object PublishSettings {
  type Sett = Def.Setting[_]

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
       releaseStepCommand(s"++${ScalaSettings.Scala211}"),
       releaseStepCommand(build.nativeTestId + "/run"),
       setReleaseVersion,
       commitReleaseVersion,
       tagRelease,
       publishArtifacts,
       releaseStepCommand(s"++${ScalaSettings.Scala211}"),
       releaseStepCommand(build.nativeParentId + "/publishSigned"),
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
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    }
}
