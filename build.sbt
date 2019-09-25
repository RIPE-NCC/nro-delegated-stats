name := "nro-combined-stats"

version := "0.1"

scalaVersion := "2.12.8"


libraryDependencies += "net.ripe.ipresource" % "ipresource" % "1.46"
libraryDependencies += "com.google.guava" % "guava" % "28.1-jre"

libraryDependencies += "net.ripe.commons" % "commons-ip-math" % "1.23"

libraryDependencies += "com.lihaoyi" %% "requests" % "0.2.0"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.6"
libraryDependencies += "com.typesafe" % "config" % "1.3.4"

libraryDependencies += "com.github.daddykotex" %% "courier" % "2.0.0"



libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies += "org.jvnet.mock-javamail" % "mock-javamail" % "1.9" % Test

// SBT bug OOM prevention according to error logs.
ThisBuild / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary