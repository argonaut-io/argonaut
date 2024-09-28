import sbt.*
import Keys.*
import org.ensime.EnsimeKeys.*

object ScalaSettings {
  type Sett = Def.Setting[?]

  private[this] val unusedWarnings = Def.setting {
    Seq("-Ywarn-unused:imports")
  }

  def Scala212 = "2.12.20"
  def Scala213 = "2.13.15"
  def Scala3 = "3.3.4"

  lazy val all: Seq[Sett] = Def.settings(
    scalaVersion := Scala213,
    crossScalaVersions := Seq(Scala212, Scala213, Scala3),
    ensimeScalaVersion := Scala212,
    scalacOptions ++= {
      if (build.isScala3.value) {
        Seq(
        )
      } else {
        Seq(
          unusedWarnings.value,
          Seq(
            "-Xlint"
          )
        ).flatten
      }
    },
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-feature",
      "-language:implicitConversions,higherKinds"
    ),
    scalacOptions ++= {
      if (scalaBinaryVersion.value == "2.13") {
        Seq("-Wconf:msg=method are copied from the case class constructor:silent")
      } else {
        Nil
      }
    },
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) =>
          Seq("-Xsource:3")
        case _ =>
          Nil
      }
    },
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11 | 12)) =>
          Seq("-Xfuture")
        case _ =>
          Nil
      }
    }
  ) ++ Seq(Compile, Test).flatMap(c => (c / console / scalacOptions) --= unusedWarnings.value)
}
