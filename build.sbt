import pl.project13.scala.sbt.JmhPlugin

ThisBuild / organization := "me.kright"
ThisBuild / version := "0.3.1"
ThisBuild / scalaVersion := "3.8.3"

ThisBuild / description := "Lightweight and efficient multi-dimensional array views for Scala"
ThisBuild / homepage := Some(url("https://github.com/kright/ArrayView"))
ThisBuild / startYear := Some(2025)

ThisBuild / developers := List(
  Developer(
    id = "Kright",
    name = "Igor Slobodskov",
    email = "simplicivy@gmail.com",
    url = url("https://kright.me/about")
  )
)

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/kright/ArrayView"),
    "scm:git@github.com:kright/ArrayView.git"
  )
)

ThisBuild / licenses := List("MIT" -> url("https://opensource.org/licenses/MIT"))

ThisBuild / sonatypeCredentialHost := "central.sonatype.com"
ThisBuild / sonatypeRepository := "https://central.sonatype.com/service/local"
ThisBuild / sonatypeProfileName := "me.kright"

lazy val sonatypeSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
    if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
    else sonatypePublishToBundle.value
  }
)

lazy val scalatestSettings = Seq(
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.20" % "test",
  libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-19" % "3.2.20.0" % "test",
)

lazy val compilerFlags =
  scalacOptions ++= Seq(
    "-Yexplicit-nulls",
    "-Werror",
    "-Wunused:all",
  )

lazy val root = (project in file("."))
  .settings(sonatypeSettings)
  .settings(
    name := "arrayview-root",
    publish / skip := true,
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
  .settings(sonatypeSettings)

lazy val benchmark = project
  .in(file("benchmark"))
  .enablePlugins(JmhPlugin)
  .settings(compilerFlags)
  .settings(
    name := "arrayview-benchmark",
    publish / skip := true,
    scalaVersion := "3.8.3",
    libraryDependencies += "org.openjdk.jmh" % "jmh-core" % "1.37",
    libraryDependencies += "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.37"
  )
  .dependsOn(arrayview.jvm)
