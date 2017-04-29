import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import Tools.onVersion
import sbtrelease.ReleasePlugin
import sbtrelease.ReleasePlugin.autoImport._
import com.typesafe.tools.mima.plugin.MimaPlugin._
import com.typesafe.tools.mima.plugin.MimaKeys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSOptimizerOptions
import sbtcrossproject.{CrossProject, Platform}
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import scalanative.sbtplugin.ScalaNativePlugin.autoImport._

object build {
  type Sett = Def.Setting[_]

  val base = ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion              = "7.2.12"
  val paradiseVersion            = "2.1.0"
  val monocleVersion             = "1.4.0"
  val catsVersion                = "0.9.0"
  val scalacheckVersion          = "1.13.5"

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
    , libraryDependencies ++= reflect(scalaOrganization.value, scalaVersion.value)
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

  val jvmSettings = Seq[Sett](
    libraryDependencies ++= Seq(
      "org.scalacheck"               %%  "scalacheck"                % scalacheckVersion        % "test"
    , "org.specs2"                   %%  "specs2-scalacheck"         % "3.8.9"                  % "test"
    )
  )

  def argonautCrossProject(name: String, platforms: Seq[Platform]) = {
    val p = CrossProject(name, file(name), CrossType.Full, platforms: _*)
      .settings(commonSettings)
      .jvmSettings(jvmSettings)
      .jsSettings(
        scalaJSOptimizerOptions ~= { options =>
          // https://github.com/scala-js/scala-js/issues/2798
          try {
            scala.util.Properties.isJavaAtLeast("1.8")
            options
          } catch {
            case _: NumberFormatException =>
              options.withParallel(false)
          }
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
