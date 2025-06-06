import sbt.*
import Keys.*
import com.jsuereth.sbtpgp.PgpKeys.*
import sbtrelease.ReleasePlugin.autoImport.*

object PublishSettings {
  type Sett = Def.Setting[?]

  lazy val all = Seq[Sett](
    pom,
    publish,
    publishMavenStyle := true,
    Test / publishArtifact := false,
    pomIncludeRepository := { _ => false },
    releasePublishArtifactsAction := publishSigned.value,
    releaseProcess := {
      import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations.*
      Seq[ReleaseStep](
        checkSnapshotDependencies,
        inquireVersions,
        runTest,
        setReleaseVersion,
        commitReleaseVersion,
        tagRelease,
        releaseStepCommandAndRemaining("+ publishSigned"),
        releaseStepCommandAndRemaining("sonaRelease"),
        setNextVersion,
        commitNextVersion,
        pushChanges
      )
    },
    releaseCrossBuild := true,
    licenses := Seq("BSD-3-Clause" -> url("http://www.opensource.org/licenses/BSD-3-Clause")),
    homepage := Some(url("https://github.com/argonaut-io/argonaut")),
    autoAPIMappings := true,
    apiURL := Some(url("https://javadoc.io/doc/io.github.argonaut-io/argonaut_2.13/latest/argonaut/index.html"))
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
    publishTo := (if (isSnapshot.value) None else localStaging.value)
}
