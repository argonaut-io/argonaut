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

  val scalazVersion              = "7.3.5"
  val monocleVersion             = "3.1.0"
  val catsVersion                = "2.6.1"

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

  val previousVersions = settingKey[Seq[String]]("")

  def nativeTestId = "nativeTest"
  def nativeParentId = "nativeParent"

  val nativeSettings = Seq(
    crossScalaVersions -= ScalaSettings.Scala3,
    previousVersions --= (0 to 2).map("6.3." + _),
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
        val last = 7
        if (isScala3.value) {
          (7 to last).map(n => s"6.3.$n")
        } else {
          (0 to last).map(n => s"6.3.$n")
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
    , specs2Version := {
        if (isScala3.value) {
          "5.0.0-RC-11"
        } else {
          "4.12.2"
        }
      }
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
        libraryDependencies ++= {
          if (isScala3.value) {
            Nil
          } else {
            Seq("com.chuusai" %%% "shapeless" % "2.3.7" % "test")
          }
        },
        mimaPreviousArtifacts := {
          previousVersions.value.map { n =>
            organization.value %% Keys.name.value % n
          }.toSet
        },
        libraryDependencies += "org.specs2" %%% "specs2-scalacheck" % specs2Version.value % "test",
      )
      .platformsSettings(platforms.filter(NativePlatform != _): _*)(
        scalacheckVersion := "1.15.3",
        libraryDependencies ++= {
          if (isScala3.value) {
            Nil
          } else {
            Seq("com.chuusai" %%% "shapeless" % "2.3.7" % "test")
          }
        },
        libraryDependencies ++= Seq(
          "org.scalaz" %%% "scalaz-core" % scalazVersion % "test" cross CrossVersion.for3Use2_13
        )
      )
    
    val withJS = if (platforms.contains(JSPlatform)) {
      p.jsSettings(
        Test / parallelExecution := false,
        mimaPreviousArtifacts := previousVersions.value.map { n =>
          organization.value %% s"${Keys.name.value}_sjs1" % n
        }.toSet,
        libraryDependencies ++= {
          Seq("org.specs2" %%% "specs2-scalacheck" % specs2Version.value % "test")
        },
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
