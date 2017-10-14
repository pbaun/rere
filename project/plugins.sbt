scalacOptions += "-deprecation"

//Generators

//Dev
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

//Debug

//Build
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC12")
addSbtPlugin("com.dwijnand" % "sbt-travisci" % "1.1.1")
