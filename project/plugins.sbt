addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.3")

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions += "-deprecation"

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.8")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.3.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.23")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.5.0")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.5.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.7")

addSbtPlugin("org.ensime" % "sbt-ensime" % "2.5.1")
