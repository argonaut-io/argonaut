addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC10")

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions += "-deprecation"

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.6")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.17")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.19")

addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.1")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.2")
