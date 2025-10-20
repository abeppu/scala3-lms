ThisBuild / scalaVersion := "3.5.2"

scalacOptions ++= Seq("-experimental")
libraryDependencies ++= Seq(
  "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
  "org.scala-lang" %% "scala3-staging" % scalaVersion.value
)
libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.19" % "test")
