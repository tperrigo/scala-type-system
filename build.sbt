name := "scala-type-system"

organization := "tperrigo"

version := "1.0.0"

scalaVersion := "2.11.8"

sbtVersion := "0.13.12"

resolvers += Resolver.sonatypeRepo("releases")
//
//resolvers += Resolver.jcenterRepo

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions ++= Seq(
  "-Yinline-warnings",
  "-Xfatal-warnings",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-encoding", "UTF-8",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:existentials",
  "-deprecation",
  "-unchecked",
  "-Xcheckinit",
  "-Xlint",
  "-Xverify",
  "-Xfuture",
  "-Yclosure-elim",
  "-Yinline",
  "-Yno-adapted-args")

lazy val shapelessVerion = "2.3.2"
lazy val scalacticVersion = "3.0.0"
lazy val scalatestVersion = "3.0.0"
lazy val simulacrumVersion = "0.10.0"

libraryDependencies ++= Seq("com.chuusai" %% "shapeless" % shapelessVerion,
  "org.scalactic" %% "scalactic" % scalacticVersion,
  "com.github.mpilquist" %% "simulacrum" % simulacrumVersion,
  "org.scalatest" %% "scalatest" % scalatestVersion % "test")
