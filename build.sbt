import AssemblyKeys._

import com.typesafe.startscript.StartScriptPlugin

name := "Scalabha"

version := "0.2.5"

organization := "OpenNLP"

scalaVersion := "2.9.1"

crossPaths := false

retrieveManaged := true

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2",
  "repo.codahale.com" at "http://repo.codahale.com",
  "Breeze Maven2" at "http://repo.scalanlp.org/repo",
  "Clojars" at "http://www.clojars.org/repo"
)

libraryDependencies ++= Seq(
  "org.apache.opennlp" % "opennlp-tools" % "1.5.2-incubating",
  "org.clapper" %% "argot" % "0.3.8",
  "net.sf.opencsv" % "opencsv" % "2.1",
  "com.codecommit" %% "anti-xml" % "0.3",
  "com.codahale" %% "jerkson" % "0.5.0",
  "org.scalanlp" % "breeze-learn_2.9.2" % "0.1-SNAPSHOT",
  "org.apache.commons" % "commons-lang3" % "3.0.1",
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16",
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...

seq(assemblySettings: _*)

jarName in assembly := "scalabha-assembly.jar"

test in assembly := {}

seq(StartScriptPlugin.startScriptForClassesSettings: _*)

