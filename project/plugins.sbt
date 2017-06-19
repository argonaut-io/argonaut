addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M15")

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions += "-deprecation"

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.4")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.14")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.16")

addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.1.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.0")
