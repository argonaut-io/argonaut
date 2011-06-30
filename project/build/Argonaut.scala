import sbt._

class Argonaut(info: ProjectInfo) extends DefaultProject(info) {
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

  val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" withSources

  val scalaz = "org.scalaz" %% "scalaz-core" % "6.0.1" withSources

  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}
