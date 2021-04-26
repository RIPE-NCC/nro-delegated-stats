package net.ripe.rpki.nro.service

import java.io.{File, PrintWriter}
import scala.io.Source.fromResource

import com.github.tototoshi.csv.{CSVReader, CSVWriter, DefaultCSVFormat}
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Const._
import net.ripe.rpki.nro.iana.IanaMagic
import net.ripe.rpki.nro.model.{AsnRecord, Conflict, Ipv4Record, Ipv6Record, Record, Records}

import scala.util.{Success, Try, Using}
import requests.Response

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import java.security.MessageDigest
import java.math.BigInteger

import net.ripe.rpki.nro.service.Ports.md5digest
/**
 * Importing data from remotes files and exporting to file.
 * Ports from Port & Adapter/Hexagonal architecture, i.e stuff on the edge communicating with outer world.
 */
object Ports extends Logging {

  // Success definition for retrying requests
  implicit val okResponse: retry.Success[Response] = retry.Success[Response](_.statusCode ==  200)

  implicit object PipeFormat extends DefaultCSVFormat {
    override val delimiter = '|'
  }

  val comments: List[String] => Boolean = _.head.startsWith("#")
  val summary : List[String] => Boolean = _.last == "summary"
  val version: String => Boolean = s => s.forall(c => c.isDigit || c == '.')
  val header: List[String] => Boolean = line => version(line.head)

  val isRecord: List[String] => Boolean = line => !(comments(line) || header(line) || summary(line))

  def parseRecordFile(source: String): Records = {
    logger.info(s"Parsing local source $source")
    Using.resource(CSVReader.open(source))(reader => toRecords(reader.all()))
  }

  def parseRecordSource(source: String): Records = {
    logger.info(s"Parsing local source $source")
    val src = fromResource(source)
    Using.resource(CSVReader.open(src))(reader => toRecords(reader.all()))
  }

  def toRecords(lines : List[List[String]]): Records = {
    var asn  : Vector[AsnRecord]  = Vector[AsnRecord]  ()
    var ipv4 : Vector[Ipv4Record] = Vector[Ipv4Record] ()
    var ipv6 : Vector[Ipv6Record] = Vector[Ipv6Record] ()

    val records = lines.withFilter(isRecord).map(Record.apply)

    records.foreach {
      case rec: AsnRecord   => asn  = asn  :+ rec
      case rec: Ipv4Record  => ipv4 = ipv4 :+ rec
      case rec: Ipv6Record  => ipv6 = ipv6 :+ rec
    }

    Records(asn.toList, ipv4.toList, ipv6.toList)
  }

  def fetchWithRetries(target: String) = {
    retry.JitterBackoff(max = maxRetries).apply(() => Future {
      logger.info(s"--Fetch with retry $target --")
      requests.get(target)
    })
  }
  // Fetch only if data file is not yet downloaded.
  def fetchLocally(source: String, dest: String) {

    if (new File(dest).isFile) {
      logger.info(s"     File $dest exist, not fetching")
      return
    }
    logger.info(s"---Fetching $source into $dest---")

    // No Md5 for iana, only fetch source
    if(source.contains("iana")){
          val ianaResponse : Future[Response] = fetchWithRetries(source)
          Try(Await.result(ianaResponse, Duration.Inf)) match {
             case Success(response) if response.statusCode == 200 =>
              if(response.contents.length < 2000){
                logger.error(s"Contents $source is too small, please investigate manually.")
                System.exit(1)
              }
              val responseText = response.text()
              Using.resource(new PrintWriter(new File(dest))) { writer =>
                writer.write(responseText)
                logger.info(s"---Done fetching $source into $dest---\n\n\n")
              }
            case _ =>
              logger.error(s"Failed to fetch $source after $maxRetries retries")
              System.exit(1)
          }
    } else {
      val sourceAttempts = fetchWithRetries(source)
      val md5Attempts    = fetchWithRetries(s"${source}.md5")

      val sourceMd5 : Future[(Response, Response)]   = for {
        sourceResponse ← sourceAttempts
        md5Response    ← md5Attempts
      } yield (sourceResponse, md5Response)

      Try(Await.result(sourceMd5, Duration.Inf)) match {
        case Success((response, md5response)) if response.statusCode == 200 =>
          if(response.contents.length < 2000){
            logger.error(s"Contents $source is too small, please investigate manually.")
            System.exit(1)
          }
          val responseText = response.text()
          val maybeMD5 = md5response.text().split(" ").filter(_.length == 32).headOption

          if(maybeMD5.isDefined){
            val computedMd5 = md5digest(responseText)
            val retrievedMd5 = maybeMD5.get
            if(computedMd5 != retrievedMd5){
              logger.error(s"MD5 does not match!")
              logger.error(s"    Computed : $computedMd5")
              logger.error(s"    Retrieved: $retrievedMd5")
              System.exit(1)
            }
          }
          logger.info("MD5 Match for "+ source)
          Using.resource(new PrintWriter(new File(dest))) { writer =>
            writer.write(responseText)
            logger.info(s"---Done fetching $source into $dest---\n\n\n")
          }
        case _ =>
          logger.error(s"Failed to fetch $source after $maxRetries retries")
          System.exit(1)
      }
    }
  }

  def fetchAndParse(ownmagic: Boolean = true): (Iterable[Records], Records, Option[Records], List[Conflict], Records) = {

    val allowedListRecords = allowedList match {
      case Left(path) => parseRecordFile(path)
      case Right(resource) => parseRecordSource(resource)
    }

    val recordMaps: Map[String, Records] = sources.map {
      case (name:String, url:String) =>
        fetchLocally(url, s"${config.currentDataDirectory}/$name")
        name -> parseRecordFile(s"${config.currentDataDirectory}/$name")
    }

    val rirs = (recordMaps - "iana").view.mapValues(_.formatRIRs).values
    val iana = if(ownmagic)  IanaMagic.processIanaRecords else recordMaps("iana")
    logger.info(if(ownmagic)"Using own magic" else "Using NRO iana from geoff")

    val allSourceFileContainSomething = iana.size > 0 && rirs.forall(_.size > 0)
    if(!allSourceFileContainSomething){
      logger.error(s"Please check the input files in ${config.currentDataDirectory} some of them are empty.")
      System.exit(1)
    }

    val previousResult = Try(parseRecordFile(s"${config.previousResultFile}")).toOption
    val oldConflict = Try(readConflicts(s"${config.previousConflictFile}")).getOrElse(List())
    (rirs, iana, previousResult, oldConflict, allowedListRecords)
  }

  def writeRecords(records: Records, outputFile: String = s"$resultFileName", header: Boolean = true): Unit = {
      writeResult(records.asn, records.ipv4, records.ipv6, outputFile, header)
  }

  def writeResult(asn: List[Record], ipv4: List[Record], ipv6: List[Record], outputFile: String = s"$resultFileName",
                  header:Boolean = true              ) {
    Using.resource(new PrintWriter(new File(outputFile))) { writer =>

      val totalSize = asn.size + ipv4.size + ipv6.size
      if(totalSize == 0) return
      if(header){
        writer.write(s"2|nro|${config.CURRENT_DAY}|$totalSize|$MAGIC_SERIAL_NUMBER|${config.CURRENT_DAY}|+0000\n")
        writer.write(s"nro|*|asn|*|${asn.size}|summary\n")
        writer.write(s"nro|*|ipv4|*|${ipv4.size}|summary\n")
        writer.write(s"nro|*|ipv6|*|${ipv6.size}|summary\n")
      }

      writer.write(asn.map(_.toString).mkString("\n"))
      writer.write("\n")
      writer.write(ipv4.map(_.toString).mkString("\n"))
      writer.write("\n")
      writer.write(ipv6.map(_.toString).mkString("\n"))
    }
  }

  def writeClaims(recs : Records, fileName: String): Unit = writeResult(recs.asn, recs.ipv4, recs.ipv6, fileName, header = false)

  def writeConflicts(conflicts: List[Conflict], outputFile: String = s"${config.currentConflictFile}"): Unit = {
    Using.resource( CSVWriter.open(new File(outputFile))) { writer =>
      conflicts.foreach(c => {
        writer.writeRow(c.a.asList)
        writer.writeRow(c.b.asList)
        writer.writeRow(List())
      })
    }
  }

  def readConflicts(conflictFile: String = s"${config.previousConflictFile}"): List[Conflict] = {
    logger.debug(s"Reading conflicts from ${config.previousConflictFile}")
    Using.resource(CSVReader.open(new File(conflictFile))){ reader =>
      reader.all()
        .filter(_.size>1)
        .sliding(2,2).map {
        case List(a,b) => Conflict(Record(a),Record(b))
      }.toList
    }
  }

  def md5digest(text:String): String = {
    val digest: Array[Byte] = MessageDigest.getInstance("MD5").digest(text.getBytes)
    val md5 = new BigInteger(1, digest).toString(16)
    // pad with leading zero
    md5.reverse.padTo(32, '0').reverse
  }
}
