name := "Scalabha"

version := "0.1"

organization := "OpenNLP"

scalaVersion := "2.9.0"

crossPaths := false

retrieveManaged := true

seq(sbtassembly.Plugin.assemblySettings: _*)

jarName in Assembly := "scalabha-assembly.jar"

