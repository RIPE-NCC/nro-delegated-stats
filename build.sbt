import Dependencies._

version := "0.1"
organization := "net.ripe.nro.stat"
name := "nro-delegated-stats"

scalaVersion := "2.13.1"
libraryDependencies ++= mainDeps ++ logConfDeps ++ testDeps
mainClass in assembly := Some("net.ripe.rpki.nro.Main")
assemblyJarName in assembly := "nro-delegated-stats.jar"