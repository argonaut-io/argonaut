import sbt._
import Keys._
import com.jsuereth.sbtpgp.PgpKeys._
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
import dotty.tools.sbtplugin.DottyPlugin.autoImport.isDottyJS

object build {
  type Sett = Def.Setting[_]

  val isScala3 = Def.setting(
    CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 3)
  )

  val base = ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion              = "7.3.3"
  val monocleVersion             = "1.7.3"
  val catsVersion                = "2.5.0"

  val scalacheckVersion          = settingKey[String]("")
  val specs2Version              = settingKey[String]("")

  val reflect = Def.setting(
    if (isScala3.value) {
      Nil
    } else {
      Seq(scalaOrganization.value % "scala-reflect" % scalaVersion.value)
    }
  )

  private[this] val tagName = Def.setting {
    s"v${if (releaseUseGlobalVersion.value) (ThisBuild / version).value else version.value}"
  }

  private[this] val tagOrHash = Def.setting {
    if(isSnapshot.value) {
      sys.process.Process("git rev-parse HEAD").lineStream_!.head
    } else {
      tagName.value
    }
  }

  private[this] val previousVersions = (0 to 0).map(n => s"6.3.$n")

  def nativeTestId = "nativeTest"
  def nativeParentId = "nativeParent"

  val nativeSettings = Seq(
    Test / sources := Nil // disable native test
  )

  val commonSettings = base ++
    ReplSettings.all ++
    ReleasePlugin.projectSettings ++
    PublishSettings.all ++
    Def.settings(
      addCommandAlias("SetScala3", s"++ ${PublishSettings.Scala3}!"),
      Seq(Compile, Test).map { scope =>
        (scope / unmanagedSourceDirectories) += {
          val base = baseDirectory.value.getParentFile / "shared" / "src"
          val dir = base / Defaults.nameForSrc(scope.name)
          if (isScala3.value) {
            dir / "scala3"
          } else {
            dir / "scala2"
          }
        }
      }
    , (Compile / doc / scalacOptions) ++= {
        val tag = tagOrHash.value
        val base = (LocalRootProject / baseDirectory).value.getAbsolutePath
        if (isScala3.value) {
          Nil
        } else {
          Seq("-sourcepath", base, "-doc-source-url", "https://github.com/argonaut-io/argonaut/tree/" + tag + "€{FILE_PATH}.scala")
        }
      }
    , (Compile / doc / sources) := {
        val src = (Compile / doc / sources).value
        if (isScala3.value) {
          Nil
        } else {
          src
        }
      }
    , releaseTagName := tagName.value
    , libraryDependencies ++= reflect.value
    , specs2Version := "4.10.6"
    , ThisBuild / mimaReportSignatureProblems := true
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
        Test / fork := true,
        (Test / baseDirectory) := (LocalRootProject / baseDirectory).value
      )
      .jvmSettings(
        mimaPreviousArtifacts := {
          previousVersions.map { n =>
            organization.value %% Keys.name.value % n
          }.toSet
        }
      )
      .platformsSettings(platforms.filter(NativePlatform != _): _*)(
        scalacheckVersion := "1.15.3",
        libraryDependencies ++= Seq(
            "org.scalaz"               %%% "scalaz-core"               % scalazVersion            % "test"
          , "org.scalacheck"           %%% "scalacheck"                % scalacheckVersion.value  % "test"
          , "org.specs2"               %%% "specs2-scalacheck"         % specs2Version.value      % "test"
        )
      )
    
    val withJS = if (platforms.contains(JSPlatform)) {
      p.jsSettings(
        Test / parallelExecution := false,
        mimaPreviousArtifacts := previousVersions.map { n =>
          organization.value %% s"${Keys.name.value}_sjs1" % n
        }.toSet,
        scalacOptions += {
          val a = (LocalRootProject / baseDirectory).value.toURI.toString
          val g = "https://raw.githubusercontent.com/argonaut-io/argonaut/" + tagOrHash.value
          val key = CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((3, _)) =>
              "-scalajs-mapSourceURI"
            case _ =>
              "-P:scalajs:mapSourceURI"
          }
          s"${key}:$a->$g/"
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
