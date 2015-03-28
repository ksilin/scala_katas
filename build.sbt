name := "katas"

version := "1.0"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.2" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1"
