import com.typesafe.startscript.StartScriptPlugin

name := "Scalabha"

version := "0.2.5"

organization := "OpenNLP"

scalaVersion := "2.9.2"

crossPaths := false

retrieveManaged := true

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2",
  "repo.codahale.com" at "http://repo.codahale.com"
)

libraryDependencies ++= Seq(
  "org.apache.opennlp" % "opennlp-tools" % "1.5.2-incubating",
  "org.clapper" % "argot_2.9.1" % "0.3.8",
  "net.sf.opencsv" % "opencsv" % "2.1",
  "com.codecommit" % "anti-xml_2.9.1" % "0.3",
  "com.codahale" % "jerkson_2.9.1" % "0.5.0",
  "org.scalanlp" % "breeze-learn_2.9.2" % "0.2-SNAPSHOT" changing(),
  "org.apache.commons" % "commons-lang3" % "3.0.1",
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16",
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...

seq(StartScriptPlugin.startScriptForClassesSettings: _*)

