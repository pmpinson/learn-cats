productName := "measurement"
name := "learn-cats"
releaseVersion := "1.0.0"

jdkVersion := "1.8"
scalaVersion := "2.13.10"


libraryDependencies ++= Seq(
  "org.typelevel" % "cats-effect_2.13" % "3.3.14",

//  "org.scalatest" %% "scalatest-shouldmatchers" % scalaTestVersion % Test,
//  "org.scalatest" %% "scalatest-freespec" % scalaTestVersion % Test,
//  "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test,
)
publishTestArtifacts
