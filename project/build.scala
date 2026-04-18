import sbt.*
import Keys.*
import com.jsuereth.sbtpgp.PgpKeys.*
import sbtprojectmatrix.ProjectMatrixKeys.*
import sbtrelease.ReleasePlugin
import sbtrelease.ReleasePlugin.autoImport.*
import com.typesafe.tools.mima.plugin.MimaPlugin.*
import com.typesafe.tools.mima.plugin.MimaKeys.*
import scalanative.sbtplugin.ScalaNativePlugin.autoImport.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*

object build {
  type Sett = Def.Setting[?]

  val isScala3 = Def.setting(
    CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 3)
  )

  val base = ScalaSettings.all ++ Seq[Sett](
    organization := "io.github.argonaut-io"
  )

  val scalazVersion = "7.3.8"
  val monocleVersion = "3.3.0"
  val catsVersion = "2.13.0"

  private def lastVersion: String = "6.3.12"

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

  val jvmSettings = Def.settings(
    // https://github.com/scala/scala-parser-combinators/issues/197
    // https://github.com/sbt/sbt/issues/4609
    Test / fork := true,
    Test / baseDirectory := (LocalRootProject / baseDirectory).value,
    mimaPreviousArtifacts := Set(
      organization.value %% Keys.name.value % lastVersion
    ),
    Seq(Compile, Test).map(c =>
      c / unmanagedSourceDirectories ++= Seq(
        projectMatrixBaseDirectory.value.getAbsoluteFile / "jvm" / "src" / Defaults.nameForSrc(c.name) / "scala",
        projectMatrixBaseDirectory.value.getAbsoluteFile / "jvm-native" / "src" / Defaults.nameForSrc(c.name) / "scala",
      ),
    ),
  )

  val jsSettings = Def.settings(
    mimaPreviousArtifacts := Set(
      organization.value %% s"${Keys.name.value}_sjs1" % lastVersion
    ),
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
    },
    Seq(Compile, Test).map(c =>
      c / unmanagedSourceDirectories ++= {
        val base = projectMatrixBaseDirectory.value.getAbsoluteFile
        Seq(
          base / "jvm-js" / "src" / Defaults.nameForSrc(c.name) / "scala",
          base / "js-native" / "src" / Defaults.nameForSrc(c.name) / "scala",
        ) ++ (
          scalaBinaryVersion.value match {
            case "3" =>
              Seq(
                base / "jvm-js" / "src" / Defaults.nameForSrc(c.name) / "scala-3",
                base / "js-native" / "src" / Defaults.nameForSrc(c.name) / "scala-3",
              )
            case _ =>
              Seq(
                base / "jvm-js" / "src" / Defaults.nameForSrc(c.name) / "scala-2",
                base / "js-native" / "src" / Defaults.nameForSrc(c.name) / "scala-2",
              )
          }
        )
      }
    )
  )

  val nativeSettings = Def.settings(
    mimaPreviousArtifacts := Set(
      organization.value %% s"${Keys.name.value}_native0.5" % lastVersion
    ),
    Seq(Compile, Test).map(c =>
      c / unmanagedSourceDirectories ++= {
        val base = projectMatrixBaseDirectory.value.getAbsoluteFile
        Seq(
          base / "jvm-native" / "src" / Defaults.nameForSrc(c.name) / "scala",
          base / "js-native" / "src" / Defaults.nameForSrc(c.name) / "scala",
        ) ++ (
          scalaBinaryVersion.value match {
            case "3" =>
              Seq(
                base / "jvm-native" / "src" / Defaults.nameForSrc(c.name) / "scala-3",
                base / "js-native" / "src" / Defaults.nameForSrc(c.name) / "scala-3",
              )
            case _ =>
              Seq(
                base / "jvm-native" / "src" / Defaults.nameForSrc(c.name) / "scala-2",
                base / "js-native" / "src" / Defaults.nameForSrc(c.name) / "scala-2",
              )

          }
        )
      }
    ),
  )

  val commonSettings = Def.settings(
    TaskKey[(Int, Int)]("checkSourceEmpty") := (
      (Compile / sources).value.size,
      (Test / sources).value.size
    ),
    base,
    ReplSettings.all,
    ReleasePlugin.projectSettings,
    PublishSettings.all,
    Seq(Compile, Test).map(c =>
      c / unmanagedSourceDirectories += {
        projectMatrixBaseDirectory.value.getAbsoluteFile / "shared" / "src" / Defaults.nameForSrc(
          c.name
        ) / s"scala-${scalaBinaryVersion.value}"
      }
    ),
    Test / parallelExecution := false,
    Seq(Compile, Test).map(c =>
      c / unmanagedSourceDirectories ++= {
        projectMatrixBaseDirectory.?.value.toSeq.flatMap { x =>
          val d = x.getAbsoluteFile / "shared" / "src" / Defaults.nameForSrc(c.name)
          val d1 = d / "scala"

          d1 +: (
            CrossVersion.partialVersion(scalaVersion.value) match {
              case Some((n, _)) =>
                Seq(d / s"scala-${n}")
              case _ =>
                Nil
            }
          )
        }
      },
    ),
    (Compile / doc / scalacOptions) ++= {
      val tag = tagOrHash.value
      val base = (LocalRootProject / baseDirectory).value.getAbsolutePath
      if (isScala3.value) {
        Seq(
          "-source-links:github://argonaut-io/argonaut",
          "-revision",
          tag
        )
      } else {
        Seq(
          "-sourcepath",
          base,
          "-doc-source-url",
          "https://github.com/argonaut-io/argonaut/tree/" + tag + "€{FILE_PATH}.scala"
        )
      }
    },
    releaseTagName := tagName.value,
    libraryDependencies ++= reflect.value,
    mimaReportSignatureProblems := (scalaBinaryVersion.value != "3"),
    libraryDependencies += {
      scalaBinaryVersion.value match {
        case "3" =>
          "org.specs2" %%% "specs2-scalacheck" % "4.23.0" % "test"
        case _ =>
          "org.specs2" %%% "specs2-scalacheck" % "4.23.0" % "test"
      }
    },
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
}
