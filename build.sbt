organization in ThisBuild := "rere"

lazy val compilerOptions = Seq(
  "-target:jvm-1.8",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

lazy val baseSettings = Seq(
  scalacOptions := compilerOptions ++ (
    // Some imports are used only on scala 2.11; on 2.12 they are not used
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, p)) if p == 11 => Seq("-Ywarn-unused-import")
      case _ => Nil
    }
  ),
  scalacOptions in (Compile, console) ~= {
    _.filterNot(Set("-Ywarn-unused-import"))
  },
  scalacOptions in (Test, console) ~= {
    _.filterNot(Set("-Ywarn-unused-import"))
  },
  scalacOptions in Test ~= {
    _.filterNot(Set("-Ywarn-dead-code"))
  }
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)


val akkaVersion = "2.4.17"
val circeVersion = "0.7.0"
val catsVersion = "0.9.0"

lazy val parboiled    = "org.parboiled"          %% "parboiled"                   % "2.1.4"             // Apache 2

lazy val shapeless    = "com.chuusai"            %% "shapeless"                   % "2.3.2"             // Apache 2

lazy val circeCore    = "io.circe"               %% "circe-core"                  % circeVersion        // Apache 2
lazy val circeGeneric = "io.circe"               %% "circe-generic"               % circeVersion        // Apache 2
lazy val circeParser  = "io.circe"               %% "circe-parser"                % circeVersion        // Apache 2
lazy val circeLiteral = "io.circe"               %% "circe-literal"               % circeVersion        // Apache 2
lazy val circeShapes  = "io.circe"               %% "circe-shapes"                % circeVersion        // Apache 2

lazy val cats         = "org.typelevel"          %% "cats"                        % catsVersion         // MIT

lazy val akkaActor    = "com.typesafe.akka"      %% "akka-actor"                  % akkaVersion         // ApacheV2
lazy val akkaStream   = "com.typesafe.akka"      %% "akka-stream"                 % akkaVersion         // ApacheV2

lazy val scalatest    = "org.scalatest"          %% "scalatest"                   % "3.0.1"             // ApacheV2
lazy val scalamock    = "org.scalamock"          %% "scalamock-scalatest-support" % "3.5.0"             // MIT
lazy val akkaTK       = "com.typesafe.akka"      %% "akka-testkit"                % akkaVersion         // ApacheV2
lazy val akkaStreamTK = "com.typesafe.akka"      %% "akka-stream-testkit"         % akkaVersion         // ApacheV2


val modules = file("modules")

lazy val sasl = (project in modules / "sasl")
  .settings(baseSettings)
  .settings(
    description := "rere sasl",
    moduleName := "rere-sasl",
    name := "sasl"
  )
  .settings(
    libraryDependencies ++= Seq(
      parboiled,
      scalatest % Test,
      scalamock % Test
    )
  )

lazy val ql = (project in modules / "ql")
  .settings(baseSettings)
  .settings(
    description := "rere ql",
    moduleName := "rere-ql",
    name := "ql"
  )
  .settings(
    sourceGenerators in Compile += ProtoGenerator.gen.taskValue
  )
  .settings(
    libraryDependencies ++= Seq(
      shapeless,
      circeCore,
      circeGeneric,
      circeParser,
      circeLiteral,
      circeShapes,
      cats,
      akkaActor,
      scalatest % Test
    )
  )


lazy val driver = (project in modules / "driver")
  .dependsOn(sasl, ql)
  .configs(IntegrationTest)
  .settings(Defaults.itSettings : _*)
  .settings(baseSettings)
  .settings(
    description := "rere driver",
    moduleName := "rere-driver",
    name := "driver"
  )
  .settings(
    libraryDependencies ++= Seq(
      circeCore,
      circeGeneric,
      circeParser,
      circeLiteral,
      circeShapes,
      cats,
      akkaActor,
      akkaStream,
      scalatest % "test,it",
      scalamock % Test,
      akkaTK % Test,
      akkaStreamTK % Test
    )
  )

lazy val example = (project in modules / "example")
  .dependsOn(driver)
  .settings(baseSettings)
  .settings(
    description := "rere example",
    moduleName := "rere-example",
    name := "example"
  )
  .settings(noPublishSettings:_*)

lazy val rere = (project in file("."))
  .aggregate(sasl, ql, driver)
  .dependsOn(sasl, ql, driver)
  .settings(baseSettings)
  .settings(noPublishSettings:_*)
