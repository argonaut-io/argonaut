import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import Tools.onVersion
import sbtrelease.ReleasePlugin
import sbtrelease.ReleasePlugin.autoImport._
import com.typesafe.tools.mima.plugin.MimaPlugin._
import com.typesafe.tools.mima.plugin.MimaKeys._
import sbtcrossproject.{CrossProject, Platform}
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.{toScalaJSGroupID => _, _}
import scalanative.sbtplugin.ScalaNativePlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.isScalaJSProject

object build {
  type Sett = Def.Setting[_]

  val base = ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion              = "7.2.19"
  val paradiseVersion            = "2.1.0"
  val monocleVersion             = "1.5.0"
  val catsVersion                = "1.0.1"
  val scalacheckVersion          = "1.13.5"

  val enableScalaJSTests         = settingKey[Boolean]("")
  val specs2Version              = settingKey[String]("")

  def reflect(o: String, v: String) =
                                    Seq(o % "scala-reflect"  % v) ++
           (if (v.contains("2.10")) Seq("org.scalamacros" %% "quasiquotes" % paradiseVersion) else Seq())

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
    Seq(addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.patch)) ++
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
    , libraryDependencies ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, v)) if v >= 13 =>
            Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.1.2" % "test")
          case _ =>
            Seq()
        }
      }
    , libraryDependencies ++= reflect(scalaOrganization.value, scalaVersion.value)
    , specs2Version := {
        if (enableScalaJSTests.value)
          "4.0.3"
        else
          "3.9.1"
      }
    , enableScalaJSTests := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, v)) =>
            v >= 11
          case _ =>
            false
        }
      }
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
      .platformsSettings(JVMPlatform, JSPlatform)(
        libraryDependencies ++= {
          if(!isScalaJSProject.value || enableScalaJSTests.value) {
            Seq(
              "org.scalaz"               %%% "scalaz-core"               % scalazVersion            % "test"
            , "org.scalacheck"           %%% "scalacheck"                % scalacheckVersion        % "test"
            , "org.specs2"               %%% "specs2-scalacheck"         % specs2Version.value      % "test"
            )
          } else Nil
        }
      )
      .jsSettings(
        parallelExecution in Test := false,
        sources in Test := {
          if(enableScalaJSTests.value)
            (sources in Test).value
          else
            Nil
        },
        scalacOptions += {
          val a = (baseDirectory in LocalRootProject).value.toURI.toString
          val g = "https://raw.githubusercontent.com/argonaut-io/argonaut/" + tagOrHash.value
          s"-P:scalajs:mapSourceURI:$a->$g/"
        }
      )

    if (platforms.contains(NativePlatform))
      p.nativeSettings(nativeSettings)
    else
      p
  }
}
