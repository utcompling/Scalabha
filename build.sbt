import AssemblyKeys._

name := "Scalabha"

version := "0.2.2"

organization := "OpenNLP"

scalaVersion := "2.9.1"

crossPaths := false

retrieveManaged := true

libraryDependencies ++= Seq(
  "org.clapper" %% "argot" % "0.3.5",
  "org.apache.commons" % "commons-lang3" % "3.0.1",
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16",
  "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...

seq(assemblySettings: _*)

jarName in assembly := "scalabha-assembly.jar"
