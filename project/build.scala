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

object build {
  type Sett = Def.Setting[_]

  val isScala3 = Def.setting(
    CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 3)
  )

  val base = ScalaSettings.all ++ Seq[Sett](
      organization := "io.argonaut"
  )

  val scalazVersion              = "7.3.6"
  val monocleVersion             = "3.1.0"
  val catsVersion                = "2.6.1"

  private def lastVersion = 8

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

  val previousVersions = settingKey[Seq[String]]("")

  def nativeTestId = "nativeTest"

  val nativeSettings = Seq(
    Compile / doc / scalacOptions --= {
      // TODO remove this workaround
      // https://github.com/scala-native/scala-native/issues/2503
      if (scalaBinaryVersion.value == "3") {
        (Compile / doc / scalacOptions).value.filter(_.contains("-Xplugin"))
      } else {
        Nil
      }
    },
    previousVersions --= {
      val last = {
        if (scalaBinaryVersion.value == "3") {
          if (name.value == "argonaut") {
            6
          } else if (name.value == "argonaut-scalaz") {
            7
          } else {
            lastVersion
          }
        } else if (name.value == "argonaut-jawn") {
          6
        } else if (name.value == "argonaut-cats") {
          7
        } else {
          2
        }
      }
      (0 to last).map("6.3." + _),
    },
    mimaPreviousArtifacts := previousVersions.value.map { n =>
      organization.value %% s"${Keys.name.value}_native0.4" % n
    }.toSet,
    Test / sources := Nil // disable native test
  )

  val commonSettings = base ++
    ReplSettings.all ++
    ReleasePlugin.projectSettings ++
    PublishSettings.all ++
    Def.settings(
      (Compile / doc / scalacOptions) ++= {
        val tag = tagOrHash.value
        val base = (LocalRootProject / baseDirectory).value.getAbsolutePath
        if (isScala3.value) {
          Nil
        } else {
          Seq("-sourcepath", base, "-doc-source-url", "https://github.com/argonaut-io/argonaut/tree/" + tag + "€{FILE_PATH}.scala")
        }
      }
    , previousVersions := {
        if (isScala3.value) {
          (7 to lastVersion).map(n => s"6.3.$n")
        } else {
          (0 to lastVersion).map(n => s"6.3.$n")
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
    , mimaReportSignatureProblems := true
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
      .jvmSettings(
        // https://github.com/scala/scala-parser-combinators/issues/197
        // https://github.com/sbt/sbt/issues/4609
        Test / fork := true,
        (Test / baseDirectory) := (LocalRootProject / baseDirectory).value,
        mimaPreviousArtifacts := {
          previousVersions.value.map { n =>
            organization.value %% Keys.name.value % n
          }.toSet
        },
      )
      .platformsSettings(platforms.filter(NativePlatform != _): _*)(
        libraryDependencies += {
          if (isScala3.value) {
            "org.specs2" %%% "specs2-scalacheck" % "5.0.0" % "test"
          } else {
            "org.specs2" %%% "specs2-scalacheck" % "4.13.3" % "test"
          }
        },
        libraryDependencies ++= {
          if (isScala3.value) {
            Nil
          } else {
            Seq("com.chuusai" %%% "shapeless" % "2.3.8" % "test")
          }
        },
        libraryDependencies ++= Seq(
          "org.scalaz" %%% "scalaz-core" % scalazVersion % "test"
        )
      )
      .jsSettings(
        Test / parallelExecution := false,
        mimaPreviousArtifacts := previousVersions.value.map { n =>
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

    if (platforms.contains(NativePlatform)) {
      p.nativeSettings(nativeSettings)
    } else {
      p
    }
  }
}
