scalacOptions += "-deprecation"

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.13")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.7.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.32")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.6.1")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.9")

addSbtPlugin("org.ensime" % "sbt-ensime" % "2.6.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.2")
