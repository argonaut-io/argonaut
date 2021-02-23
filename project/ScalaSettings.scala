import sbt._
import Keys._
import org.ensime.EnsimeKeys._
import dotty.tools.sbtplugin.DottyPlugin.autoImport.{isDotty, dottyLatestNightlyBuild}

object ScalaSettings {
  type Sett = Def.Setting[_]

  private[this] val unusedWarnings = Def.setting {
    Seq("-Ywarn-unused:imports")
  }

  def Scala212 = "2.12.13"

  lazy val all: Seq[Sett] = Def.settings(
    scalaVersion := Scala212
  , commands += Command.command("SetDottyNightlyVersion") {
      s"""++ ${dottyLatestNightlyBuild.get}!""" :: _
    }
  , crossScalaVersions := Seq(Scala212, "2.13.5")
  , ensimeScalaVersion := Scala212
  , test / fork := true
  , scalacOptions ++= {
      if (isDotty.value) {
        Seq(
          "-Xignore-scala2-macros"
        )
      } else {
        Seq(
          unusedWarnings.value,
          Seq(
            "-Xlint"
          )
        ).flatten
      }
    }
  , scalacOptions ++= Seq(
      "-deprecation"
    , "-unchecked"
    , "-feature"
    , "-language:implicitConversions,higherKinds"
    )
  , scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11 | 12)) =>
          Seq("-Xfuture")
        case _ =>
          Nil
      }
    }
  ) ++ Seq(Compile, Test).flatMap(c =>
    (c / console / scalacOptions) --= unusedWarnings.value
  )
}
