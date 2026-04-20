ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "Project2ParallelComputing"
  )

libraryDependencies += "org.scala-lang.modules"%% "scala-parallel-collections" %  "1.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test