addSbtPlugin(("com.typesafe.sbt" % "sbt-pgp" % "0.7").cross(CrossVersion.full))

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.2.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.7")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.4")
