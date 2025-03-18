import sbt.*
import Keys.*
import com.jsuereth.sbtpgp.PgpKeys.*
import sbtrelease.ReleasePlugin
import sbtrelease.ReleasePlugin.autoImport.*
import com.typesafe.tools.mima.plugin.MimaPlugin.*
import com.typesafe.tools.mima.plugin.MimaKeys.*
import sbtcrossproject.CrossProject
import sbtcrossproject.Platform
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*
import scalanative.sbtplugin.ScalaNativePlugin.autoImport.*
import scalanativecrossproject.ScalaNativeCrossPlugin.autoImport.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*

object build {
  type Sett = Def.Setting[?]

  def oldGroupId: String = "io.argonaut"

  val isScala3 = Def.setting(
    CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 3)
  )

  val base = ScalaSettings.all ++ Seq[Sett](
    organization := "io.github.argonaut-io"
  )

  val scalazVersion = "7.3.8"
  val monocleVersion = "3.3.0"
  val catsVersion = "2.13.0"

  private def lastVersion = 10

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
    if (isSnapshot.value) {
      sys.process.Process("git rev-parse HEAD").lineStream_!.head
    } else {
      tagName.value
    }
  }

  val previousVersions = settingKey[Seq[String]]("")

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
      val last = 9
      (0 to last).map("6.3." + _),
    },
    mimaPreviousArtifacts := previousVersions.value.map { n =>
      oldGroupId %% s"${Keys.name.value}_native0.5" % n
    }.toSet,
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
          Seq(
            "-sourcepath",
            base,
            "-doc-source-url",
            "https://github.com/argonaut-io/argonaut/tree/" + tag + "€{FILE_PATH}.scala"
          )
        }
      },
      previousVersions := {
        if (isScala3.value) {
          (7 to lastVersion).map(n => s"6.3.$n")
        } else {
          (0 to lastVersion).map(n => s"6.3.$n")
        }
      },
      (Compile / doc / sources) := {
        val src = (Compile / doc / sources).value
        if (isScala3.value) {
          Nil
        } else {
          src
        }
      },
      releaseTagName := tagName.value,
      libraryDependencies ++= reflect.value,
      mimaReportSignatureProblems := (scalaBinaryVersion.value != "3")
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
    val p = CrossProject(name, file(name))(platforms*)
      .crossType(CrossType.Full)
      .settings(commonSettings)
      .jvmSettings(
        // https://github.com/scala/scala-parser-combinators/issues/197
        // https://github.com/sbt/sbt/issues/4609
        Test / fork := true,
        (Test / baseDirectory) := (LocalRootProject / baseDirectory).value,
        mimaPreviousArtifacts := {
          previousVersions.value.map { n =>
            oldGroupId %% Keys.name.value % n
          }.toSet
        },
      )
      .settings(
        libraryDependencies += {
          scalaBinaryVersion.value match {
            case "3" =>
              "org.specs2" %%% "specs2-scalacheck" % "4.21.0" % "test"
            case _ =>
              "org.specs2" %%% "specs2-scalacheck" % "4.21.0" % "test"
          }
        },
      )
      .settings(
        libraryDependencies ++= {
          if (isScala3.value) {
            Nil
          } else {
            Seq("com.chuusai" %%% "shapeless" % "2.3.13" % "test")
          }
        },
        libraryDependencies ++= Seq(
          "org.scalaz" %%% "scalaz-core" % scalazVersion % "test"
        )
      )
      .jsSettings(
        Test / parallelExecution := false,
        mimaPreviousArtifacts := previousVersions.value.map { n =>
          oldGroupId %% s"${Keys.name.value}_sjs1" % n
        }.toSet,
        if (sys.props.isDefinedAt("scala_js_wasm")) {
          Def.settings(
            scalaJSLinkerConfig ~= (_.withExperimentalUseWebAssembly(true).withModuleKind(ModuleKind.ESModule)),
            jsEnv := {
              import org.scalajs.jsenv.nodejs.NodeJSEnv
              val config = NodeJSEnv
                .Config()
                .withArgs(
                  List(
                    "--experimental-wasm-exnref",
                    "--experimental-wasm-imported-strings",
                    "--turboshaft-wasm",
                  )
                )
              new NodeJSEnv(config)
            },
          )
        } else {
          Def.settings()
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

    if (platforms.contains(NativePlatform)) {
      p.nativeSettings(nativeSettings)
    } else {
      p
    }
  }
}
