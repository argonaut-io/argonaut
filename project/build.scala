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
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import dotty.tools.sbtplugin.DottyPlugin.autoImport.{isDotty, isDottyJS}

object build {
  type Sett = Def.Setting[_]

  val base = ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion              = "7.3.2"
  val monocleVersion             = "1.7.3"
  val catsVersion                = "2.2.0"

  val scalacheckVersion          = settingKey[String]("")
  val specs2Version              = settingKey[String]("")

  val reflect = Def.setting(
    if (isDotty.value) {
      Nil
    } else {
      Seq(scalaOrganization.value % "scala-reflect" % scalaVersion.value)
    }
  )

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

  private[this] val previousVersions = (0 to 0).map(n => s"6.3.$n")

  val commonSettings = base ++
    ReplSettings.all ++
    ReleasePlugin.projectSettings ++
    PublishSettings.all ++
    Def.settings(
      Seq(Compile, Test).map { scope =>
        unmanagedSourceDirectories in scope += {
          val base = baseDirectory.value.getParentFile / "shared" / "src"
          val dir = base / Defaults.nameForSrc(scope.name)
          if (isDotty.value) {
            dir / "scala3"
          } else {
            dir / "scala2"
          }
        }
      }
    , scalacOptions in (Compile, doc) ++= {
        val tag = tagOrHash.value
        val base = (baseDirectory in LocalRootProject).value.getAbsolutePath
        if (isDotty.value) {
          Nil
        } else {
          Seq("-sourcepath", base, "-doc-source-url", "https://github.com/argonaut-io/argonaut/tree/" + tagOrHash.value + "€{FILE_PATH}.scala")
        }
      }
    , sources in (Compile, doc) := {
        val src = (sources in (Compile, doc)).value
        if (isDotty.value) {
          Nil
        } else {
          src
        }
      }
    , releaseTagName := tagName.value
    , libraryDependencies ++= reflect.value
    , specs2Version := "4.10.5"
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
        fork in Test := true,
        baseDirectory in Test := (baseDirectory in LocalRootProject).value
      )
      .jvmSettings(
        mimaPreviousArtifacts := {
          previousVersions.map { n =>
            organization.value %% Keys.name.value % n
          }.toSet
        }
      )
      .settings(
        scalacheckVersion := "1.15.1",
        libraryDependencies ++= Seq(
            "org.scalaz"               %%% "scalaz-core"               % scalazVersion            % "test"
          , "org.scalacheck"           %%% "scalacheck"                % scalacheckVersion.value  % "test"
          , "org.specs2"               %%% "specs2-scalacheck"         % specs2Version.value      % "test"
        )
      )
    
    if (platforms.contains(JSPlatform)) {
      p.jsSettings(
        parallelExecution in Test := false,
        mimaPreviousArtifacts := previousVersions.map { n =>
          organization.value %% s"${Keys.name.value}_sjs1" % n
        }.toSet,
        scalacOptions ++= {
          if (isDottyJS.value) {
            // TODO
            // https://github.com/lampepfl/dotty/blob/4c99388e77be12ee6cc/compiler/src/dotty/tools/backend/sjs/JSPositions.scala#L64-L69
            Nil
          } else {
            val a = (baseDirectory in LocalRootProject).value.toURI.toString
            val g = "https://raw.githubusercontent.com/argonaut-io/argonaut/" + tagOrHash.value
            Seq(s"-P:scalajs:mapSourceURI:$a->$g/")
          }
        }
      )
    } else {
      p
    }
  }
}
