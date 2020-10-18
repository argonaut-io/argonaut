scalacOptions += "-deprecation"

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.13")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.8.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.2.0")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

addSbtPlugin("org.ensime" % "sbt-ensime" % "2.6.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.4")

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.4.2")
