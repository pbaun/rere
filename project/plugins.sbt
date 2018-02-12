scalacOptions += "-deprecation"

//Generators

//Dev
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

//Debug

//Build
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.2")
addSbtPlugin("com.dwijnand" % "sbt-travisci" % "1.1.1")

//Publish
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.7")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")
