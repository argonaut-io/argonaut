import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import sbtrelease.ReleasePlugin
import sbtrelease.ReleasePlugin.autoImport._
import com.typesafe.tools.mima.plugin.MimaPlugin._
import com.typesafe.tools.mima.plugin.MimaKeys._
import sbtcrossproject.{CrossProject, Platform}
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object build {
  type Sett = Def.Setting[_]

  val base = ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion              = "7.2.30"
  val monocleVersion             = Def.setting(
    "1.6.0"
  )
  val catsVersion                = "2.0.0"

  val scalacheckVersion          = settingKey[String]("")
  val specs2Version              = settingKey[String]("")

  def reflect(o: String, v: String) = Seq(o % "scala-reflect"  % v)

  private[this] val tagName = Def.setting {
    s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
  }

  private[this] val tagOrHash = Def.setting {
    if(isSnapshot.value) {
      sys.process.Process("git rev-parse HEAD").lineStream_!.head
    } else {
      tagName.value
    }
  }

  val commonSettings = base ++
    ReplSettings.all ++
    ReleasePlugin.projectSettings ++
    PublishSettings.all ++
    Seq[Sett](
      scalacOptions += "-language:_"
    , scalacOptions in (Compile, doc) ++= {
        val base = (baseDirectory in LocalRootProject).value.getAbsolutePath
        Seq("-sourcepath", base, "-doc-source-url", "https://github.com/argonaut-io/argonaut/tree/" + tagOrHash.value + "€{FILE_PATH}.scala")
      }
    , releaseTagName := tagName.value
    , resolvers += Resolver.sonatypeRepo("releases")
    , resolvers += Resolver.sonatypeRepo("snapshots")
    , autoScalaLibrary := false
    , libraryDependencies ++= reflect(scalaOrganization.value, scalaVersion.value)
    , specs2Version := "4.8.3"
    // no mima until 6.2.0 release.
    , mimaPreviousArtifacts := Set()
    /*
    , mimaBinaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      /* adding functions to sealed traits is binary incompatible from java, but ok for scala, so ignoring */
      Seq(
      ) map exclude[MissingMethodProblem]
    }
    */
  )

  def argonautCrossProject(name: String, platforms: Seq[Platform]) = {
    val p = CrossProject(name, file(name))(platforms: _*)
      .crossType(CrossType.Full)
      .settings(commonSettings)
      .platformsSettings(JVMPlatform)(
        // https://github.com/scala/scala-parser-combinators/issues/197
        // https://github.com/sbt/sbt/issues/4609
        fork in Test := true,
        baseDirectory in Test := (baseDirectory in LocalRootProject).value
      )
      .settings(
        scalacheckVersion := "1.14.3",
        libraryDependencies ++= Seq(
            "org.scalaz"               %%% "scalaz-core"               % scalazVersion            % "test"
          , "org.scalacheck"           %%% "scalacheck"                % scalacheckVersion.value  % "test"
          , "org.specs2"               %%% "specs2-scalacheck"         % specs2Version.value      % "test"
        )
      )
    
    if (platforms.contains(JSPlatform)) {
      p.jsSettings(
        parallelExecution in Test := false,
        scalacOptions += {
          val a = (baseDirectory in LocalRootProject).value.toURI.toString
          val g = "https://raw.githubusercontent.com/argonaut-io/argonaut/" + tagOrHash.value
          s"-P:scalajs:mapSourceURI:$a->$g/"
        }
      )
    } else {
      p
    }
  }
}
