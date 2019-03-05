scalacOptions += "-deprecation"

//Generators

//Dev
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

//Debug

//Build
addSbtCoursier
addSbtPlugin("com.dwijnand" % "sbt-travisci" % "1.1.3")

//Publish
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.11")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.2")
