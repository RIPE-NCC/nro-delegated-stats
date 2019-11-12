package net.ripe.rpki.nro

import java.io.File
import java.time.LocalDate

import com.typesafe.config.{Config, ConfigFactory}
import courier.Mailer
import net.ripe.rpki.nro.Const.{AFRINIC, APNIC, ARIN, GEOFF, IANA, LACNIC, RIPENCC}
import org.slf4j.LoggerFactory

class Configs(todayDate: LocalDate) {
  import Configs._

  def CURRENT_DAY: String = formatDate(todayDate)

  def PREV_RESULT_DAY: String = formatDate(todayDate.minusDays(1))
  def PREV_CONFLICT_DAY: String = formatDate(todayDate.minusDays(gracePeriod))

  def currentResultFile: String = s"$resultDirectory/$CURRENT_DAY/$resultFileName"
  def currentUnclaimedFile: String = s"$resultDirectory/$CURRENT_DAY/$unclaimedFileName"
  def currentOverclaimedFile: String = s"$resultDirectory/$CURRENT_DAY/$overclaimedFileName"
  def currentMergedFile: String = s"$resultDirectory/$CURRENT_DAY/$mergedFileName"
  def previousResultFile: String = s"$resultDirectory/$PREV_RESULT_DAY/$resultFileName"
  def currentConflictFile: String = s"$resultDirectory/$CURRENT_DAY/$conflictFileName"
  def previousConflictFile: String = s"$resultDirectory/$PREV_CONFLICT_DAY/$conflictFileName"

  def currentDataDirectory: File = createIfNeeded(s"$dataDirectory/$CURRENT_DAY")
  def currentResultDirectory: File = createIfNeeded(s"$resultDirectory/$CURRENT_DAY")

  def formatDate(date: LocalDate): String = date.toString.replaceAll("-", "")

  def createIfNeeded(path: String) = {
    val resultFile = new File(s"$path")
    if(!resultFile.exists()){
      resultFile.mkdir()
    }
    resultFile
  }
}

object Configs {
  private val conf: Config = ConfigFactory.load()
  val data: Config = conf.getConfig("data")
  val sender : String = conf.getString("sender")
  val contacts: Map[String, String] = Map(
    RIPENCC -> conf.getString("ripencc.contact"),
    APNIC -> conf.getString("apnic.contact"),
    ARIN -> conf.getString("arin.contact"),
    AFRINIC -> conf.getString("afrinic.contact"),
    LACNIC -> conf.getString("lacnic.contact")
  )

  val gracePeriod: Int = conf.getInt("grace.period")
  val dataDirectory: String = conf.getString("data.directory")
  val resultDirectory: String = conf.getString("result.directory")
  val resultFileName: String = conf.getString("result.fileName")
  val mergedFileName: String = conf.getString("merged.fileName")
  val conflictFileName: String = conf.getString("conflict.fileName")
  val unclaimedFileName: String = conf.getString("unclaimed.fileName")
  val overclaimedFileName: String = conf.getString("overclaimed.fileName")
  val sources: Map[String, String] = Map[String, String](
    APNIC   -> data.getString(APNIC  ),
    AFRINIC -> data.getString(AFRINIC),
    ARIN    -> data.getString(ARIN   ),
    LACNIC  -> data.getString(LACNIC ),
    RIPENCC -> data.getString(RIPENCC),
    IANA    -> data.getString(IANA   ),
    GEOFF   -> data.getString(GEOFF  )
  )
  val mail: Config = conf.getConfig("mail")
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


  var config = new Configs(java.time.LocalDate.now)
}

trait Logging {
  val logger = LoggerFactory.getLogger(getClass.getName)
}
