package net.ripe.rpki.nro

import java.io.File
import java.time.LocalDate

import com.typesafe.config.{Config, ConfigFactory}
import courier.Mailer
import net.ripe.rpki.nro.Defs.{AFRINIC, APNIC, ARIN, GEOFF, IANA, LACNIC, RIPENCC}

object Settings {

  private val config: Config = ConfigFactory.load()

  val sender : String = config.getString("sender")
  val contacts: Map[String, String] = Map(
    RIPENCC -> config.getString("ripencc.contact"),
    APNIC -> config.getString("apnic.contact"),
    ARIN -> config.getString("arin.contact"),
    AFRINIC -> config.getString("afrinic.contact"),
    LACNIC -> config.getString("lacnic.contact")
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

  val todayDataDirectory: File = createIfNeeded(s"$dataDirectory/$TODAY")
  val todayResultDirectory: File = createIfNeeded(s"$resultDirectory/$TODAY")

  val mail: Config = config.getConfig("mail")
  val host: String = mail.getString("host")
  val port: Int = mail.getInt("port")
  val username: String = mail.getString("username")
  val password: String = mail.getString("password")
  val tls: Boolean = mail.getBoolean("tls")
  val debug: Boolean = mail.getBoolean("debug")
  val auth: Boolean = mail.getBoolean("auth")

  val mailer = Mailer(host, port)
    .as(username, password)
    .auth(auth)
    .startTls(tls)
    .debug(debug)()

  val data: Config = config.getConfig("data")

  val sources: Map[String, String] = Map[String, String](
    APNIC   -> data.getString(APNIC  ),
    AFRINIC -> data.getString(AFRINIC),
    ARIN    -> data.getString(ARIN   ),
    LACNIC  -> data.getString(LACNIC ),
    RIPENCC -> data.getString(RIPENCC),
    IANA    -> data.getString(IANA   ),
    GEOFF   -> data.getString(GEOFF  )
  )

  def formatDate(date: LocalDate): String = date.toString.replaceAll("-", "")

  def createIfNeeded(path: String) = {
    val resultFile = new File(s"$path")
    if(!resultFile.exists()){
      resultFile.mkdir()
    }
    resultFile
  }
}
