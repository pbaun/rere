scalacOptions += "-deprecation"

//Generators

//Dev
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")

//Debug

//Build
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC3")
addSbtPlugin("com.dwijnand" % "sbt-travisci" % "1.1.0")
