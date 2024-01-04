import sbt._
import Keys._
import com.jsuereth.sbtpgp.SbtPgp.autoImport.PgpKeys._
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

  val scalazVersion              = "7.2.35"
  val monocleVersion             = Def.setting(
    crossProjectPlatform.?.value match {
      case Some(JVMPlatform) | None =>
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, v)) if v <= 12 =>
            "1.6.0-M1"
          case _ =>
            "1.6.0"
        }
      case Some(JSPlatform) =>
        "1.6.3"
      case Some(p) =>
        sys.error(p.toString)
    }
  )
  val catsVersion = Def.setting(
    "2.6.1"
  )

  def reflect = Def.setting {
    if (scalaBinaryVersion.value == "3") {
      Nil
    } else {
      Seq(scalaOrganization.value % "scala-reflect" % scalaVersion.value)
    }
  }

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

  def nativeTestId = "nativeTest"
  def nativeParentId = "nativeParent"

  val nativeSettings = Seq(
    (Test / sources) := Nil // disable native test
  )

  val commonSettings = base ++
    ReplSettings.all ++
    ReleasePlugin.projectSettings ++
    PublishSettings.all ++
    Seq[Sett](
      scalacOptions += "-language:_"
    , (Compile / doc / scalacOptions) ++= {
        val base = (LocalRootProject / baseDirectory).value.getAbsolutePath
        Seq("-sourcepath", base, "-doc-source-url", "https://github.com/argonaut-io/argonaut/tree/" + tagOrHash.value + "€{FILE_PATH}.scala")
      }
    , releaseTagName := tagName.value
    , libraryDependencies ++= reflect.value
    , ThisBuild / mimaReportSignatureProblems := true
    , mimaPreviousArtifacts := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, _)) =>
            Set("6.2.6").map(
              organization.value %%% name.value % _
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
      .platformsSettings(JVMPlatform)(
        // https://github.com/scala/scala-parser-combinators/issues/197
        // https://github.com/sbt/sbt/issues/4609
        (Test / fork) := true,
        (Test / baseDirectory) := (LocalRootProject / baseDirectory).value
      )
      .platformsSettings(platforms.filter(NativePlatform != _): _*)(
        libraryDependencies += {
          scalaBinaryVersion.value match {
            case "3" =>
              "org.specs2" %%% "specs2-scalacheck" % "5.4.2" % "test"
            case _ =>
              "org.specs2" %%% "specs2-scalacheck" % "4.20.4" % "test"
          }
        },
        libraryDependencies ++= Seq(
          "org.scalaz" %%% "scalaz-scalacheck-binding" % s"${scalazVersion}-scalacheck-1.15" % "test"
        ),
        Test / baseDirectory := (LocalRootProject / baseDirectory).value,
        libraryDependencies ++= {
          if (scalaBinaryVersion.value == "3") {
            Nil
          } else {
            Seq("com.chuusai" %%% "shapeless" % "2.3.10" % "test")
          }
        },
      )
    
    val withJS = if (platforms.contains(JSPlatform)) {
      p.jsSettings(
        (Test / parallelExecution) := false,
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
