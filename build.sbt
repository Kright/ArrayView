ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.7.1"

lazy val root = (project in file("."))
  .settings(
    name := "arrayview",
    idePackagePrefix := Some("com.github.kright.arrayview"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )
