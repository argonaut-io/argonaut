import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._

object PublishSettings {
  type Sett = Def.Setting[_]

  lazy val all = Seq[Sett](
    pom
  , publish
  , publishMavenStyle := true
  , publishArtifact in Test := false
  , pomIncludeRepository := { _ => false }
  , licenses := Seq("BSD-3-Clause" -> url("http://www.opensource.org/licenses/BSD-3-Clause"))
  , homepage := Some(url("http://argonaut.io"))
  , useGpg := true
  )

  lazy val pom: Sett =
    pomExtra := (
      <scm>
        <url>git@github.com:markhibberd/argonaut.git</url>
        <connection>scm:git:git@github.com:markhibberd/argonaut.git</connection>
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
    publishTo <<= version.apply(v => {
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    })
}
