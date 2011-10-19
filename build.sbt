import AssemblyKeys._

name := "Scalabha"

version := "0.1.1"

organization := "OpenNLP"

scalaVersion := "2.9.1"

crossPaths := false

retrieveManaged := true

libraryDependencies += "org.clapper" %% "argot" % "0.3.5"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.0.1"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

seq(assemblySettings: _*)

jarName in assembly := "scalabha-assembly.jar"
