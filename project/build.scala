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
import scalanative.sbtplugin.ScalaNativePlugin.autoImport._
import scalanativecrossproject.ScalaNativeCrossPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object build {
  type Sett = Def.Setting[_]

  val base = ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion              = "7.2.27"
  val monocleVersion             = "1.6.0-M1"
  val catsVersion                = "1.6.0"

  val scalacheckVersion          = settingKey[String]("")
  val specs2Version              = settingKey[String]("")

  def reflect(o: String, v: String) = Seq(o % "scala-reflect"  % v)

  private[this] val tagName = Def.setting {
    s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
  }

  private[this] val tagOrHash = Def.setting {
    if(isSnapshot.value) {
      sys.process.Process("git rev-parse HEAD").lines_!.head
    } else {
      tagName.value
    }
  }

  def nativeTestId = "nativeTest"
  def nativeParentId = "nativeParent"

  val nativeSettings = Seq(
      scalaVersion := ScalaSettings.Scala211
    , sources in Test := Nil // disable native test
    , crossScalaVersions := ScalaSettings.Scala211 :: Nil
  )

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
    , specs2Version := "4.5.1"
    , mimaPreviousArtifacts := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 11 | 12)) =>
            Set("6.2.2", "6.2.3").map(
              organization.value %% name.value % _
            )
          case _ =>
            Set.empty
        }
      }
    , mimaBinaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      Seq(
      )
    }
  )

  def argonautCrossProject(name: String, platforms: Seq[Platform]) = {
    val p = CrossProject(name, file(name))(platforms: _*)
      .crossType(CrossType.Full)
      .settings(commonSettings)
      .platformsSettings(platforms.filter(NativePlatform != _): _*)(
        scalacheckVersion := "1.14.0",
        libraryDependencies ++= Seq(
            "org.scalaz"               %%% "scalaz-core"               % scalazVersion            % "test"
          , "org.scalacheck"           %%% "scalacheck"                % scalacheckVersion.value  % "test"
          , "org.specs2"               %%% "specs2-scalacheck"         % specs2Version.value      % "test"
        )
      )
    
    val withJS = if (platforms.contains(JSPlatform)) {
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

    if (platforms.contains(NativePlatform)) {
      withJS.nativeSettings(nativeSettings)
    } else {
      withJS
    }
  }
}
