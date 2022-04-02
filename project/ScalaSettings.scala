import sbt._
import Keys._
import org.ensime.EnsimeKeys._

object ScalaSettings {
  type Sett = Def.Setting[_]

  private[this] val unusedWarnings = Def.setting {
    Seq("-Ywarn-unused:imports")
  }

  def Scala212 = "2.12.15"
  def Scala213 = "2.13.8"
  def Scala3 = "3.1.2"

  lazy val all: Seq[Sett] = Def.settings(
    scalaVersion := Scala213
  , crossScalaVersions := Seq(Scala212, Scala213, Scala3)
  , ensimeScalaVersion := Scala212
  , scalacOptions ++= {
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
