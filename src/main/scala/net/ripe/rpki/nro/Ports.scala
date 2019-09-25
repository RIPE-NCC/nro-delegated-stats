package net.ripe.rpki.nro

import java.io.{File, PrintWriter}

import com.github.tototoshi.csv.{CSVReader, CSVWriter, DefaultCSVFormat}
import net.ripe.rpki.nro.Settings._
import org.slf4j.{Logger, LoggerFactory}

// Importing data from remotes files and exporting to file.
// Or maybe Ports from Port & Adapter/Hexagonal architecture, i.e stuff on the edge.
object Ports {
  val logger: Logger = LoggerFactory.getLogger(Ports.getClass)

  implicit object PipeFormat extends DefaultCSVFormat {
    override val delimiter = '|'
  }

  def parseRecordFile(source: String): Records = {

    // What to do with header and summaries, do we want to check integrity?

    using(CSVReader.open(source)) { reader =>
      val lines: List[List[String]] = reader.all()
        .filter(!_.head.startsWith("#"))
          .drop(4)
      // FixME: Dropping header and summaries, for now.
      // Better way is to parse it and verify/report error if summary does not match with the following records.

      val records = lines.map(Record.apply)
      var asn  : Vector[AsnRecord]  = Vector[AsnRecord]  ()
      var ipv4 : Vector[Ipv4Record] = Vector[Ipv4Record] ()
      var ipv6 : Vector[Ipv6Record] = Vector[Ipv6Record] ()

      records.foreach {
        case rec: AsnRecord   => asn  = asn :+ rec
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

    using(new PrintWriter(new File(dest))) { writer =>
      writer.write(response.text())
      logger.info(s"---Done fetching $source into $dest---\n\n\n")
    }
  }

  def fetchAndParse(): (Iterable[Records], Records, List[Conflict]) = {
    val recordMaps = sources.map {
      case (name, url) =>
        fetchLocally(url, s"$todayDataDirectory/$name")
        (name, parseRecordFile(s"$todayDataDirectory/$name"))
    }
    // Adjusting and fixing record fields conforming to what is done by geoff.
    val rirs = (recordMaps - "iana" - "geoff").mapValues(_.fixRIRs).values.seq
    val iana = recordMaps("iana").fixIana
    val oldConflict = readConflicts(s"$previousConflictFile")
    (rirs, iana, oldConflict)
  }

  def writeResult(asn: List[Record], ipv4: List[Record], ipv6: List[Record], outputFile: String = s"$resultFileName") {
    using(new PrintWriter(new File(outputFile))) { writer =>

      val totalSize = asn.size + ipv4.size + ipv6.size
      val SERIAL = 19821213

      writer.write(s"2|nro|$TODAY|$totalSize|$SERIAL|$TODAY|+0000\n")
      writer.write(s"nro|*|asn|*|${asn.size}|summary\n")
      writer.write(s"nro|*|ipv4|*|${ipv4.size}|summary\n")
      writer.write(s"nro|*|ipv6|*|${ipv6.size}|summary\n")
      writer.write(asn.map(_.toString).mkString("\n"))
      writer.write("\n")
      writer.write(ipv4.map(_.toString).mkString("\n"))
      writer.write("\n")
      writer.write(ipv6.map(_.toString).mkString("\n"))
    }
  }

  def writeConflicts(conflicts: List[Conflict], outputFile: String = s"$currentConflictFile"): Unit = {
    using( CSVWriter.open(new File(outputFile))) { writer =>
      conflicts.foreach(c => {
        writer.writeRow(c.a.asList)
        writer.writeRow(c.b.asList)
        writer.writeRow(List())
      })
    }
  }

  def readConflicts(conflictFile: String = s"$previousConflictFile"): List[Conflict] = {
    logger.debug(s"Reading conflicts from $previousConflictFile")
    using(CSVReader.open(new File(conflictFile))){ reader =>
      reader.all()
        .filter(_.size>1)
        .sliding(2,2).map {
        case List(a,b) => Conflict(Record(a),Record(b))
      }.toList
    }
  }

  def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A =
    try { f(closeable) } finally { closeable.close() }

}
