package net.ripe.rpki.nro

import java.io.File
import java.time.LocalDate
import scala.concurrent.duration.Duration

import com.typesafe.config.{Config, ConfigFactory}
import courier.Mailer
import net.ripe.rpki.nro.Const._
import org.slf4j.LoggerFactory

class Configs(private val todayDate: LocalDate) {
  import Configs._

  def CURRENT_DAY: String = formatDate(todayDate)

  def PREV_RESULT_DAY: String = formatDate(todayDate.minusDays(1))
  def PREV_CONFLICT_DAY: String = formatDate(todayDate.minusDays(gracePeriod))

  def currentResultFile: String = s"$currentResultDirectory/$resultFileName"
  def currentUnclaimedFile: String = s"$currentResultDirectory/$unclaimedFileName"
  def currentOverclaimedFile: String = s"$currentResultDirectory/$overclaimedFileName"
  def currentMergedFile: String = s"$currentResultDirectory/$mergedFileName"
  def previousResultFile: String = s"$previousResultDirectory/$resultFileName"
  def currentConflictFile: String = s"$currentResultDirectory/$conflictFileName"
  def previousConflictFile: String = s"$previousResultDirectory/$conflictFileName"

  def currentDataDirectory: File = createIfNeeded(s"$dataDirectory/$CURRENT_DAY")
  def currentResultDirectory: File = createIfNeeded(s"$resultDirectory/$CURRENT_DAY")
  def previousResultDirectory: File = createIfNeeded(s"$resultDirectory/$PREV_RESULT_DAY")
  def previousConflictDirectory: File = createIfNeeded(s"$resultDirectory/$PREV_CONFLICT_DAY")

  def formatDate(date: LocalDate): String = date.toString.replaceAll("-", "")

  def createIfNeeded(path: String) = {
    val resultFile = new File(path)
    if(!resultFile.exists()){
      resultFile.mkdir()
    }
    resultFile
  }
}

object Configs {
  private implicit def asDuration(d: java.time.Duration): Duration =
    Duration.fromNanos(d.toNanos)

  def configureFor(todayDate: LocalDate) = {
    config = new Configs(todayDate)
  }
  private val conf: Config = ConfigFactory.load()
  val urls: Config = conf.getConfig("urls")
  val sender : String = conf.getString("sender")
  val contacts: Map[String, String] = Map(
    RIPENCC -> conf.getString("ripencc.contact"),
    APNIC -> conf.getString("apnic.contact"),
    ARIN -> conf.getString("arin.contact"),
    AFRINIC -> conf.getString("afrinic.contact"),
    LACNIC -> conf.getString("lacnic.contact"),
    RSCG  -> conf.getString("rscg.contact")
  )

  // Left is a path to external file, right is a source from classpath resource will be treated differently
  val allowedList: Either[String, String] = if(conf.hasPath("allowedlist")) Left(conf.getString("allowedlist")) else Right("allowedlist")
  val gracePeriod: Int = conf.getInt("grace.period")
  val maxRetries: Int = conf.getInt("http.max-retries")
  val httpTimeout: Duration = conf.getDuration("http.timeout")
  val dataDirectory: String = conf.getString("data.directory")
  val resultDirectory: String = conf.getString("result.directory")
  val resultFileName: String = conf.getString("result.fileName")
  val mergedFileName: String = conf.getString("merged.fileName")
  val conflictFileName: String = conf.getString("conflict.fileName")
  val unclaimedFileName: String = conf.getString("unclaimed.fileName")
  val overclaimedFileName: String = conf.getString("overclaimed.fileName")
  val sources: Map[String, String] = Map[String, String](
    APNIC   -> urls.getString(APNIC  ),
    AFRINIC -> urls.getString(AFRINIC),
    ARIN    -> urls.getString(ARIN   ),
    LACNIC  -> urls.getString(LACNIC ),
    RIPENCC -> urls.getString(RIPENCC),
    IANA    -> urls.getString(IANA   ),
  )

  val ianaOrgFileURL: Map[String, String] = Map[String, String](
    ASN16      -> urls.getString(ASN16),
    ASN32      -> urls.getString(ASN32),
    IPV4_ADDRESS_SPACE      -> urls.getString(IPV4_ADDRESS_SPACE),
    IPV6_ADDRESS_SPACE      -> urls.getString(IPV6_ADDRESS_SPACE),
    IPV4_RECOVERED_SPACE    -> urls.getString(IPV4_RECOVERED_SPACE),
    IPV4_REALLOCATED_SPACE  -> urls.getString(IPV4_REALLOCATED_SPACE),
    IPV4_SPECIAL_REGISTRY   -> urls.getString(IPV4_SPECIAL_REGISTRY),
    IPV6_SPECIAL_REGISTRY   -> urls.getString(IPV6_SPECIAL_REGISTRY),
    IPV6_UNICAST_ASSIGNMENT -> urls.getString(IPV6_UNICAST_ASSIGNMENT)
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
  def logger = LoggerFactory.getLogger(getClass.getName)
}
