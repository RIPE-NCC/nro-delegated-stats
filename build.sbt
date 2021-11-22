import Dependencies._

version := "0.1"
organization := "net.ripe.nro.stat"
name := "nro-delegated-stats"

scalaVersion := "2.13.7"
scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= mainDeps ++ logConfDeps ++ testDeps

assembly / mainClass := Some("net.ripe.rpki.nro.Main")
assembly / assemblyJarName := "nro-delegated-stats.jar"
