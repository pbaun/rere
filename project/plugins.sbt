scalacOptions += "-deprecation"

//Generators

//Dev
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

//Debug

//Build
addSbtPlugin("com.dwijnand" % "sbt-travisci" % "1.1.1")

//Publish
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.6")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")
