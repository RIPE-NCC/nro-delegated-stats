import sbt._

object Dependencies {

  val commonsIpMath = "net.ripe.commons" % "commons-ip-math" % "1.23"
  val guava = "com.google.guava" % "guava" % "31.0.1-jre"
  val requests = "com.lihaoyi" %% "requests" % "0.6.5"

  val scopt = "com.github.scopt" %% "scopt" % "4.0.1"
  val csvReader = "com.github.tototoshi" %% "scala-csv" % "1.3.6"
  val mailCourier = "com.github.daddykotex" %% "courier" % "3.0.1"
  val retry = "com.softwaremill.retry" %% "retry" % "0.3.3"

  val typesafeConfig = "com.typesafe" % "config" % "1.3.4"
  val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val scalatest = "org.scalatest" %% "scalatest" % "3.0.8" % Test
  val mockMail = "com.icegreen" % "greenmail" % "1.6.5" % Test

  val mainDeps: Seq[ModuleID] = Seq(scopt, commonsIpMath, guava, requests, csvReader, mailCourier, retry)
  val logConfDeps: Seq[ModuleID] = Seq(typesafeConfig, logbackClassic)
  val testDeps: Seq[ModuleID] = Seq(scalatest, mockMail)
}
