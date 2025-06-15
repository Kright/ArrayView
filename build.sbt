
ThisBuild / version := "0.1.1-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

ThisBuild / licenses := List(License.MIT)
ThisBuild / startYear := Some(2022)


lazy val scalatestSettings = Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  libraryDependencies += "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % "test",
)

lazy val root = (project in file("."))
  .settings(
    name := "arrayview",
    packageSrc / publishArtifact := true,
  ).aggregate(
    arrayview.jvm,
    arrayview.js,
  )

lazy val arrayview = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("arrayview"))
  .settings(scalatestSettings *)