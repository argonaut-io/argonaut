scalacOptions += "-deprecation"

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.2")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.13")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.6.4")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.32")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")

addSbtPlugin("org.ensime" % "sbt-ensime" % "2.6.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.8.1")
