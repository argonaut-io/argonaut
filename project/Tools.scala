import sbt._
import Keys._

object Tools {
  type Sett = Project.Setting[_]

  def onVersion[A](all: Seq[A], on292: => Seq[A] = Seq(), on210: => Seq[A] = Seq()) =
    scalaVersion(v => all ++ (if (v.contains("2.10")) on210 else on292))

  def onVersionTask[A](all: Seq[A], on292: => Seq[A] = Seq(), on210: => Seq[A] = Seq()) =
    scalaVersion.map(v => all ++ (if (v.contains("2.10")) on210 else on292))
}

