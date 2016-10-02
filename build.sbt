name := "ping-pong"

version := "0.1"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  // "-Xfatal-warnings",
  "-Xlint:_",
  // "-Xdev",
  "-Ywarn-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  // "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-deprecation",
  "-feature",
  "-language:higherKinds"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.7.2",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")
