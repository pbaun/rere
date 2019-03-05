organization in ThisBuild := "com.github.pbaun"

lazy val compilerOptions = Seq(
  "-target:jvm-1.8",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
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
      case Some((2, p)) if p == 12 => Seq("-Xlint:-unused,_")               // https://github.com/scala/scala/pull/5402
      case Some((2, p)) if p == 11 => Seq("-Xlint", "-Ywarn-unused-import")
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

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseTagComment := s"=pro Release ${(version in ThisBuild).value}",
  releaseCommitMessage := s"=pro Set version to ${(version in ThisBuild).value}",
  homepage := Some(url("https://github.com/pbaun/rere")),
  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  scmInfo := Some(
    ScmInfo(url("https://github.com/pbaun/rere"), "scm:git:git@github.com:pbaun/rere.git")
  ),
  developers := List(
    Developer("pbaun", "Pavel Baun", "baunpavel@gmail.com", url("https://github.com/pbaun"))
  ),
  autoAPIMappings := true
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)


val akkaVersion = "2.5.21"
val circeVersion = "0.11.1"
val catsVersion = "1.6.0"

lazy val parboiled    = "org.parboiled"          %% "parboiled"                   % "2.1.5"             // Apache 2

lazy val shapeless    = "com.chuusai"            %% "shapeless"                   % "2.3.3"             // Apache 2

lazy val circeCore    = "io.circe"               %% "circe-core"                  % circeVersion        // Apache 2
lazy val circeGeneric = "io.circe"               %% "circe-generic"               % circeVersion        // Apache 2
lazy val circeParser  = "io.circe"               %% "circe-parser"                % circeVersion        // Apache 2
lazy val circeLiteral = "io.circe"               %% "circe-literal"               % circeVersion        // Apache 2
lazy val circeShapes  = "io.circe"               %% "circe-shapes"                % circeVersion        // Apache 2

lazy val catsKernel   = "org.typelevel"          %% "cats-kernel"                 % catsVersion         // MIT
lazy val catsFree     = "org.typelevel"          %% "cats-free"                   % catsVersion         // MIT

lazy val akkaActor    = "com.typesafe.akka"      %% "akka-actor"                  % akkaVersion         // ApacheV2
lazy val akkaStream   = "com.typesafe.akka"      %% "akka-stream"                 % akkaVersion         // ApacheV2

lazy val logback      = "ch.qos.logback"         % "logback-classic"              % "1.2.3"             // EPL/LGPL

lazy val scalatest    = "org.scalatest"          %% "scalatest"                   % "3.0.6"             // ApacheV2
lazy val scalamock    = "org.scalamock"          %% "scalamock"                   % "4.1.0"             // MIT
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
  .settings(publishSettings)
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
  .settings(publishSettings)
  .settings(
    sourceGenerators in Compile += ProtoGenerator.gen
  )
  .settings(
    libraryDependencies ++= Seq(
      shapeless,
      circeCore,
      circeGeneric,
      circeParser,
      circeLiteral,
      circeShapes,
      catsKernel,
      catsFree,
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
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      circeCore,
      circeGeneric,
      circeParser,
      circeLiteral,
      circeShapes,
      catsKernel,
      catsFree,
      akkaActor,
      akkaStream,
      logback,
      scalatest % "test,it",
      scalamock % Test,
      akkaTK % Test,
      akkaStreamTK % Test
    )
  )

lazy val example = (project in modules / "example")
  .settings(baseSettings)
  .settings(noPublishSettings)
  .settings(
    description := "rere example",
    moduleName := "rere-example",
    name := "example"
  )
  .dependsOn(driver)

lazy val rere = (project in file("."))
  .settings(baseSettings)
  .settings(noPublishSettings)
  .aggregate(sasl, ql, driver)
  .dependsOn(sasl, ql, driver)
