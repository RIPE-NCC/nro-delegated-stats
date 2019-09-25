package net.ripe.rpki.nro

import java.io.File
import java.time.LocalDate

import com.typesafe.config.{Config, ConfigFactory}
import courier.Mailer

import scala.collection.JavaConverters._

object Settings {

  private val config: Config = ConfigFactory.load()

  val sender : String = config.getString("sender")
  val contacts: Map[String, String] = Map(
    "ripencc" -> config.getString("ripencc.contact"),
    "apnic" -> config.getString("apnic.contact"),
    "arin" -> config.getString("arin.contact"),
    "afrinic" -> config.getString("afrinic.contact"),
    "lacnic" -> config.getString("lacnic.contact")
  )

  val gracePeriod: Int = config.getInt("grace.period")
  val dataDirectory: String = config.getString("data.directory")
  val resultDirectory: String = config.getString("result.directory")
  val resultFileName: String = config.getString("result.fileName")
  val mergedFileName: String = config.getString("merged.fileName")
  val conflictFileName: String = config.getString("conflict.fileName")

  val TODAY: String = formatDate(java.time.LocalDate.now)
  val PREV_RESULT_DAY: String = formatDate(java.time.LocalDate.now.minusDays(1))
  val PREV_CONFLICT_DAY: String = formatDate(java.time.LocalDate.now.minusDays(gracePeriod))

  val currentResultFile: String = s"$resultDirectory/$TODAY/$resultFileName"
  val currentMergedFile: String = s"$resultDirectory/$TODAY/$mergedFileName"
  val previousResultFile: String = s"$resultDirectory/$PREV_RESULT_DAY/$resultFileName"
  val currentConflictFile: String = s"$resultDirectory/$TODAY/$conflictFileName"
  val previousConflictFile: String = s"$resultDirectory/$PREV_CONFLICT_DAY/$conflictFileName"

  val todayDir: File = {
    val resultFile = new File(s"$resultDirectory/$TODAY")
    if(!resultFile.exists()){
      resultFile.mkdir()
    }

    resultFile
  }

  val mail: Config = config.getConfig("mail")
  val host: String = mail.getString("host")
  val port: Int = mail.getInt("port")
  val username: String = mail.getString("username")
  val password: String = mail.getString("password")

  val mailer = Mailer(host, port)
    .as(username, password)
    .auth(true)
    .startTls(true)
    .debug(true)()

  def formatDate(date: LocalDate): String = date.toString.replaceAll("-", "")
}
