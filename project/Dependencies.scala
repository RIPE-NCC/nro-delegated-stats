import sbt._

object Dependencies {

  val commonsIpMath = "net.ripe.commons" % "commons-ip-math" % "1.23"
  val guava = "com.google.guava" % "guava" % "28.1-jre"
  val requests = "com.lihaoyi" %% "requests" % "0.2.0"

  val csvReader = "com.github.tototoshi" %% "scala-csv" % "1.3.6"
  val mailCourier = "com.github.daddykotex" %% "courier" % "2.0.0"


  val typesafeConfig = "com.typesafe" % "config" % "1.3.4"
  val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val scalatest = "org.scalatest" %% "scalatest" % "3.0.8" % Test
  val mockMail = "org.jvnet.mock-javamail" % "mock-javamail" % "1.9" % Test

  val mainDeps: Seq[ModuleID] = Seq(commonsIpMath, guava, requests, csvReader, mailCourier)
  val logConfDeps: Seq[ModuleID] = Seq(typesafeConfig, logbackClassic)
  val testDeps: Seq[ModuleID] = Seq(scalatest, mockMail)
}
