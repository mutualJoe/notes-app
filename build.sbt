ThisBuild / organization := "com.notes"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.6"

val http4sVersion   = "0.23.30"
val circeVersion    = "0.14.14"
val catsEffectVersion = "3.6.1"
val laminarVersion  = "17.2.1"

lazy val shared = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(
    name := "notes-shared",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core"    % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion,
      "io.circe" %%% "circe-parser"  % circeVersion,
      "org.scalameta" %%% "munit"    % "1.1.1" % Test
    )
  )

lazy val sharedJvm = shared.jvm
lazy val sharedJs  = shared.js

lazy val server = project
  .in(file("server"))
  .dependsOn(sharedJvm)
  .settings(
    name := "notes-server",
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-ember-server" % http4sVersion,
      "org.http4s"    %% "http4s-dsl"          % http4sVersion,
      "org.http4s"    %% "http4s-circe"        % http4sVersion,
      "org.typelevel" %% "cats-effect"         % catsEffectVersion,
      "ch.qos.logback" % "logback-classic"     % "1.5.18",
      "org.scalameta" %% "munit"               % "1.1.1" % Test
    ),
    Compile / mainClass := Some("notes.server.Main"),
    Compile / run / fork := true,
    // Serve the linked Scala.js output from the client project
    Compile / resourceGenerators += Def.task {
      val outDir = (client / Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
      val jsFile = outDir / "main.js"
      val destDir = (Compile / resourceManaged).value / "web"
      IO.createDirectory(destDir)
      val dest = destDir / "main.js"
      IO.copyFile(jsFile, dest)
      val mapSrc = outDir / "main.js.map"
      val extras =
        if (mapSrc.exists()) {
          val mapDest = destDir / "main.js.map"
          IO.copyFile(mapSrc, mapDest)
          Seq(mapDest)
        } else Seq.empty
      Seq(dest) ++ extras
    }.taskValue
  )

lazy val client = project
  .in(file("client"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(sharedJs)
  .settings(
    name := "notes-client",
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("notes.client.Main"),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo"    %%% "laminar"     % laminarVersion,
      "io.circe"     %%% "circe-core"    % circeVersion,
      "io.circe"     %%% "circe-generic" % circeVersion,
      "io.circe"     %%% "circe-parser"  % circeVersion
    )
  )

lazy val root = project
  .in(file("."))
  .aggregate(sharedJvm, sharedJs, server, client)
  .settings(
    name := "notes-app",
    publish / skip := true
  )
