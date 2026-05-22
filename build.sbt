import pl.project13.scala.sbt.JmhPlugin

ThisBuild / version := "0.2.0"

ThisBuild / scalaVersion := "3.8.3"

ThisBuild / licenses := List(License.MIT)
ThisBuild / startYear := Some(2025)


lazy val scalatestSettings = Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % "test",
  libraryDependencies += "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % "test",
)

lazy val compilerFlags =
  scalacOptions ++= Seq(
    "-Yexplicit-nulls",
    "-Werror",
    "-Wunused:all",
  )

lazy val root = (project in file("."))
  .settings(
    name := "arrayview",
    packageSrc / publishArtifact := true,
  ).aggregate(
    arrayview.jvm,
    arrayview.js,
    benchmark
  )

lazy val arrayview = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("arrayview"))
  .settings(compilerFlags)
  .settings(scalatestSettings *)

lazy val benchmark = project
  .in(file("benchmark"))
  .enablePlugins(JmhPlugin)
  .settings(compilerFlags)
  .settings(
    name := "arrayview-benchmark",
    scalaVersion := "3.8.3",
    libraryDependencies += "org.openjdk.jmh" % "jmh-core" % "1.37",
    libraryDependencies += "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.37"
  )
  .dependsOn(arrayview.jvm)

// fix for jitpack
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
publishM2Configuration := publishM2Configuration.value.withOverwrite(true)