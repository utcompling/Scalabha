name := "Scalabha"

version := "0.1"

organization := "OpenNLP"

scalaVersion := "2.9.0"

crossPaths := false

retrieveManaged := true

seq(sbtassembly.Plugin.assemblySettings: _*)

jarName in Assembly := "scalabha-assembly.jar"

libraryDependencies += "org.clapper" %% "argot" % "0.3.5"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.0.1"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"