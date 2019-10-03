package net.ripe.rpki.nro.service

import java.io.{File, PrintWriter}

import com.github.tototoshi.csv.{CSVReader, CSVWriter, DefaultCSVFormat}
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.Settings._
import net.ripe.rpki.nro.model.{AsnRecord, Conflict, Ipv4Record, Ipv6Record, Record, Records}

import scala.util.{Try, Using}

/**
 * Importing data from remotes files and exporting to file.
 * Ports from Port & Adapter/Hexagonal architecture, i.e stuff on the edge communicating with outer world.
 */
object Ports extends Logging {

  implicit object PipeFormat extends DefaultCSVFormat {
    override val delimiter = '|'
  }

  def parseRecordFile(source: String): Records = {
    logger.info(s"Parsing local source $source")
    // What to do with header and summaries, do we want to check integrity?
    Using.resource(CSVReader.open(source)) { reader =>
      val lines: List[List[String]] = reader.all()
        .filter(!_.head.startsWith("#")) // no comments
          .drop(4)
      // FixME: Dropping header and summaries, for now.
      // Better way is to parse it and verify/report error if summary does not match with the following records.

      val records = lines.map(Record.apply)
      var asn  : Vector[AsnRecord]  = Vector[AsnRecord]  ()
      var ipv4 : Vector[Ipv4Record] = Vector[Ipv4Record] ()
      var ipv6 : Vector[Ipv6Record] = Vector[Ipv6Record] ()

      records.foreach {
        case rec: AsnRecord   => asn  = asn  :+ rec
        case rec: Ipv4Record  => ipv4 = ipv4 :+ rec
        case rec: Ipv6Record  => ipv6 = ipv6 :+ rec
      }

      Records(asn.toList, ipv4.toList, ipv6.toList)
    }
  }

  // Fetch only if data file is not yet downloaded.
  def fetchLocally(source: String, dest: String) {
    if (new File(dest).isFile) {
      logger.warn(s"     File $dest exist, not fetching")
      return
    }
    logger.info(s"---Fetching $source into $dest---")
    val response = requests.get(source)

    Using.resource(new PrintWriter(new File(dest))) { writer =>
      writer.write(response.text())
      logger.info(s"---Done fetching $source into $dest---\n\n\n")
    }
  }

  def fetchAndParse(): (Iterable[Records], Records, Option[Records], List[Conflict]) = {
    val recordMaps: Map[String, Records] = sources.map {
      case (name:String, url:String) =>
        fetchLocally(url, s"$todayDataDirectory/$name")
        name -> parseRecordFile(s"$todayDataDirectory/$name")
    }

    val rirs = (recordMaps - "iana" - "geoff").view.mapValues(_.fixRIRs).values
    val iana = recordMaps("iana").fixIana

    val previousResult = Try(parseRecordFile(s"$previousResultFile")).toOption
    val oldConflict = Try(readConflicts(s"$previousConflictFile")).getOrElse(List())
    (rirs, iana, previousResult, oldConflict)
  }

  def writeRecords(records: Records, outputFile: String = s"$resultFileName", header: Boolean = true): Unit = {
      writeResult(records.asn, records.ipv4, records.ipv6, outputFile, header)
  }

  def writeResult(asn: List[Record], ipv4: List[Record], ipv6: List[Record], outputFile: String = s"$resultFileName",
                  header:Boolean = true              ) {
    Using.resource(new PrintWriter(new File(outputFile))) { writer =>

      val totalSize = asn.size + ipv4.size + ipv6.size
      val SERIAL = 19821213
      if(totalSize == 0) return
      if(header){
        writer.write(s"2|nro|$TODAY|$totalSize|$SERIAL|$TODAY|+0000\n")
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

  def writeClaims(recs : Records, fileName: String): Unit = writeResult(recs.asn, recs.ipv4, recs.ipv6, fileName, false)

  def writeConflicts(conflicts: List[Conflict], outputFile: String = s"$currentConflictFile"): Unit = {
    Using.resource( CSVWriter.open(new File(outputFile))) { writer =>
      conflicts.foreach(c => {
        writer.writeRow(c.a.asList)
        writer.writeRow(c.b.asList)
        writer.writeRow(List())
      })
    }
  }

  def readConflicts(conflictFile: String = s"$previousConflictFile"): List[Conflict] = {
    logger.debug(s"Reading conflicts from $previousConflictFile")
    Using.resource(CSVReader.open(new File(conflictFile))){ reader =>
      reader.all()
        .filter(_.size>1)
        .sliding(2,2).map {
        case List(a,b) => Conflict(Record(a),Record(b))
      }.toList
    }
  }
}
