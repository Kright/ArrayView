import pl.project13.scala.sbt.JmhPlugin

ThisBuild / version := "0.1.6-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.2"

ThisBuild / licenses := List(License.MIT)
ThisBuild / startYear := Some(2025)


lazy val scalatestSettings = Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  libraryDependencies += "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % "test",
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
    scalaVersion := "3.7.1",
    libraryDependencies += "org.openjdk.jmh" % "jmh-core" % "1.37",
    libraryDependencies += "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.37"
  )
  .dependsOn(arrayview.jvm)
