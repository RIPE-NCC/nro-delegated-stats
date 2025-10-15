package net.ripe.rpki.nro.service

import com.github.tototoshi.csv.{CSVReader, CSVWriter, DefaultCSVFormat}
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Const._
import net.ripe.rpki.nro.iana.IanaGenerator
import net.ripe.rpki.nro.model._
import net.ripe.rpki.nro.{Configs, Logging}
import requests.Response

import java.io._
import java.math.BigInteger
import java.security.MessageDigest
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.io.Source.{fromBytes, fromFile, fromResource}
import scala.util.{Failure, Success, Try, Using}

/**
 * Importing data from remotes files and exporting to file.
 * Ports from Port & Adapter/Hexagonal architecture, i.e stuff on the edge communicating with outer world.
 */
object Ports extends Logging {

  implicit object PipeFormat extends DefaultCSVFormat {
    override val delimiter = '|'
  }

  val comments: Seq[String] => Boolean = _.head.startsWith("#")
  val summary: Seq[String] => Boolean = _.last == "summary"
  val version: String => Boolean = s => s.forall(c => c.isDigit || c == '.')
  val header: Seq[String] => Boolean = line => version(line.head)

  val isRecord: Seq[String] => Boolean = line => !(comments(line) || header(line) || summary(line))

  def parseRecordSource(source: scala.io.Source): Records = {
    Using.resource(CSVReader.open(source))(reader => toRecords(reader.all()))
  }

  def parseRecordFile(source: String): Records = {
    logger.info(s"Parsing local file $source\n")
    parseRecordSource(fromFile(source))
  }

  def toRecords(lines: Seq[List[String]]): Records = {
    var asn: Vector[AsnRecord] = Vector[AsnRecord]()
    var ipv4: Vector[Ipv4Record] = Vector[Ipv4Record]()
    var ipv6: Vector[Ipv6Record] = Vector[Ipv6Record]()

    val records = lines.withFilter(isRecord).map(Record.apply)

    records.foreach {
      case rec: AsnRecord => asn = asn :+ rec
      case rec: Ipv4Record => ipv4 = ipv4 :+ rec
      case rec: Ipv6Record => ipv6 = ipv6 :+ rec
    }

    Records(asn.toList, ipv4.toList, ipv6.toList)
  }

  def fetchWithRetries(target: String): Future[Response] = {
    implicit val success: retry.Success[Response] = retry.Success[Response](_.statusCode == 200)

    retry.JitterBackoff(max = maxRetries).apply(() => Future {
      logger.info(s"-> trying $target")
      requests.get(
        url = target,
        headers = Iterable("user-agent" -> "nro-delegated-stats"),
        compress = requests.Compress.Gzip,
      )
    })
  }

  // Fetch only if data file is not yet downloaded.
  def fetchLocally(source: String, dest: String): Unit = {

    if (new File(dest).isFile) {
      logger.info(s"     File $dest exist, not fetching")
      return
    }
    logger.info(s"Fetching $source into $dest")

    // No Md5 for iana, only fetch source
    if (source.contains("iana")) {
      val ianaResponse: Future[Response] = fetchWithRetries(source)
      Try(Await.result(ianaResponse, httpTimeout)) match {
        case Success(response) if response.statusCode == 200 =>
          if (response.bytes.length < 2000) {
            logger.error(s"Contents $source is too small, please investigate manually.")
            System.exit(1)
          }
          Using.resource(new FileOutputStream(dest)) { out =>
            response.writeBytesTo(out)
            logger.info("Ok")
          }
        case Failure(err) =>
          logger.error(s"Failed to fetch $source after $maxRetries retries:\n$err\n")
          System.exit(1)
      }
    } else {
      val sourceAttempts = fetchWithRetries(source)
      val md5Attempts = fetchWithRetries(s"$source.md5")

      val sourceMd5: Future[(Response, Response)] = for {
        sourceResponse <- sourceAttempts
        md5Response <- md5Attempts
      } yield (sourceResponse, md5Response)

      Try(Await.result(sourceMd5, httpTimeout)) match {
        case Success((response, md5response)) if response.statusCode == 200 =>
          if (response.bytes.length < 2000) {
            logger.error(s"Contents $source is too small, please investigate manually.")
            System.exit(1)
          }
          // Example MD5, Arin has different formats, the rest of the MD5 looks like the first line.:
          //   MD5 (delegated-apnic-extended-latest) = 83c9ed2721a049faf70bb88fd586347d
          //   d9e9d22a6fc88d9455ccd198a061d1fc  delegated-arin-extended-20210502
          // This explains the filter.
          val maybeMD5 = md5response.text().split("\\s+").find(_.length == 32)

          if (maybeMD5.isDefined) {
            val computedMd5 = md5digest(response.bytes)
            val retrievedMd5 = maybeMD5.get
            if (computedMd5 != retrievedMd5) {
              logger.error(s"MD5 does not match! for $source")
              logger.error(s"    Computed : $computedMd5")
              logger.error(s"    Retrieved: $retrievedMd5")
              System.exit(1)
            }
            logger.info(s"-> MD5 sum for $source: $computedMd5")
          } else {
            logger.error(s"Unrecognized MD5 format for: $source : \n\t$md5response")
            System.exit(1)
          }
          Using.resource(new FileOutputStream(dest)) { out =>
            response.writeBytesTo(out)
            logger.info(s"Ok.")
          }
        case Failure(err) =>
          logger.error(s"Failed to fetch $source after $maxRetries retries:\n$err\n")
          System.exit(1)
      }
    }
  }

  def fetchAndParseInputs(baseURL: String, ownmagic: Boolean = true): (Iterable[Records], Records, Option[Records]) = {

    val recordMaps: Map[String, Records] = sources.map {
      case (name: String, url: String) =>
        fetchLocally(url, s"${config.currentDataDirectory}/$name")
        name -> parseRecordFile(s"${config.currentDataDirectory}/$name")
    }

    val rirs = (recordMaps - "iana").view.mapValues(_.formatRIRs).values
    val iana = if (ownmagic) IanaGenerator.processIanaRecords else recordMaps("iana")
    logger.info(if (ownmagic) "Using own magic" else "Using NRO iana from geoff")

    val allSourceFileContainSomething = iana.size > 0 && rirs.forall(_.size > 0)
    if (!allSourceFileContainSomething) {
      logger.error(s"Please check the input files in ${config.currentDataDirectory} some of them are empty.")
      System.exit(1)
    }

    val previousResult = Try {
      Await.result(
        fetchWithRetries(s"$baseURL/${config.PREV_RESULT_DAY}/$resultFileName")
          .map { response => parseRecordSource(fromBytes(response.bytes, "UTF-8")) },
        httpTimeout
      )
    }
    if (previousResult.isFailure) {
      logger.warn(s"Could not fetch previous results. This may impact unclaimed results.")
    }

    (rirs, iana, previousResult.toOption)
  }

  private def getAllowedList: Records =
    parseRecordSource(allowedList.fold(fromFile(_), fromResource(_)))

  def getConflicts(baseConflictURL: String): (Records, Seq[Conflict], Seq[Conflict]) = {
    def getIt(path: String) =
      Try(readConflicts(path)).getOrElse(Seq())

    val previousPath = s"$baseConflictURL/${config.PREV_CONFLICT_DAY}/${Configs.conflictFileName}"
    val currentPath = s"$baseConflictURL/${config.CURRENT_DAY}/${Configs.conflictFileName}"

    (getAllowedList, getIt(currentPath), getIt(previousPath))
  }

  def getUnclaimed(baseConflictURL: String): (Seq[Unclaimed], Seq[Unclaimed]) = {
    def getIt(path: String) =
      Try(readUnclaimed(path)).getOrElse(Seq())

    val previousPath = s"$baseConflictURL/${config.PREV_CONFLICT_DAY}/${Configs.unclaimedFileName}"
    val currentPath = s"$baseConflictURL/${config.CURRENT_DAY}/${Configs.unclaimedFileName}"

    (getIt(currentPath), getIt(previousPath))
  }

  def writeRecords(records: Records, outputFile: String = s"$resultFileName"): Unit = {
    Using.resource(new PrintWriter(new File(outputFile))) { writer =>
      writeHeader(records, writer)
      writeResult(records, writer)
    }
  }

  def writeDisclaimer(writer: PrintWriter): Unit = {
    val disclaimer = scala.io.Source.fromResource("disclaimer.txt").getLines().mkString("\n")
    writer.write(disclaimer)
    writer.write("\n")
  }

  def writeHeader(records: Records, writer: PrintWriter): Unit = {
    val (asn, ipv4, ipv6) = (records.asn, records.ipv4, records.ipv6)
    val totalSize = asn.size + ipv4.size + ipv6.size
    writer.write(s"2|nro|${config.CURRENT_DAY}|$totalSize|$MAGIC_SERIAL_NUMBER|${config.CURRENT_DAY}|+0000\n")
    writer.write(s"nro|*|asn|*|${asn.size}|summary\n")
    writer.write(s"nro|*|ipv4|*|${ipv4.size}|summary\n")
    writer.write(s"nro|*|ipv6|*|${ipv6.size}|summary\n")
  }

  def writeResult(records: Records, writer: PrintWriter): Unit = {
    val (asn, ipv4, ipv6) = (records.asn, records.ipv4, records.ipv6)
    val totalSize = asn.size + ipv4.size + ipv6.size
    if (totalSize == 0) return
    writer.write(asn.map(_.toString).mkString("\n"))
    writer.write("\n")
    writer.write(ipv4.map(_.toString).mkString("\n"))
    writer.write("\n")
    writer.write(ipv6.map(_.toString).mkString("\n"))
  }

  def writeClaims(recs: Records, fileName: String): Unit =
    Using.resource(new PrintWriter(new File(fileName))) { writer =>
      writeResult(recs, writer)
    }

  def writeConflicts(conflicts: Seq[Conflict], outputFile: String = s"${config.currentConflictFile}"): Unit = {
    Using.resource(CSVWriter.open(new File(outputFile))) { writer =>
      conflicts.foreach(c => {
        writer.writeRow(c.a.asList)
        writer.writeRow(c.b.asList)
        writer.writeRow(Nil)
      })
    }
  }

  def readConflicts(conflictURL: String): Seq[Conflict] = {
    logger.debug(s"Reading conflicts from $conflictURL")
    fetchFile(conflictURL, parseConflicts)
  }

  def readUnclaimed(unclaimedURL: String): Seq[Unclaimed] = {
    logger.debug(s"Reading unclaimed from $unclaimedURL")
    fetchFile(unclaimedURL, parseUnclaimed)
  }

  def fetchFile[S](url: String, parse: StringReader => S): S = {
    Try(Await.result(fetchWithRetries(url), httpTimeout)) match {
      case Success(response)
        if response.statusCode == 200 =>
        parse(new StringReader(response.text()))
      case _ =>
        throw new RuntimeException("Failed to fetch conflict files")
    }
  }

  def parseConflicts(csvReader: Reader): Seq[Conflict] = {
    Using.resource(CSVReader.open(csvReader)) { reader =>
      reader.all()
        .filter(_.size > 1)
        .sliding(2, 2)
        .map {
          case Seq(a, b) => Conflict(Record(a), Record(b))
          case x =>
            throw new IllegalArgumentException(s"Cannot parse incomplete conflicts file. Remainder: $x")
        }.toSeq
    }
  }

  def parseUnclaimed(csvReader: Reader): Seq[Unclaimed] = {
    Using.resource(CSVReader.open(csvReader)) { reader =>
      reader.all()
        .filter(_.size > 1)
        .map {
          case r: List[String] => Unclaimed(Record(r))
          case x =>
            throw new IllegalArgumentException(s"Cannot parse unclaimed resources file. Remainder: $x")
        }
    }
  }

  def md5digest(data: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("MD5").digest(data)
    val md5 = new BigInteger(1, digest).toString(16)
    // pad with leading zero
    md5.reverse.padTo(32, '0').reverse
  }
}
