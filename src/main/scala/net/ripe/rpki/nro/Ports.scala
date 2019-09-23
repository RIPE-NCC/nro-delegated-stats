package net.ripe.rpki.nro

import java.io.{File, PrintWriter}

import com.github.tototoshi.csv.{CSVReader, CSVWriter, DefaultCSVFormat}
import com.typesafe.config.ConfigFactory
import net.ripe.rpki.nro.Defs._

import scala.collection.immutable
import scala.io.Source.fromFile

// Importing data from remotes files and exporting to file.
// Or maybe Ports from Port & Adapter/Hexagonal architecture, i.e stuff on the edge.
object Ports {

  val config = ConfigFactory.load()

  val dataDirectory = config.getString("data.directory")
  val resultDirectory = config.getString("result.directory")

  val resultToday = s"$resultDirectory/$TODAY"

  val todayDir = {
      val resultFile = new File(resultToday)
      if(!resultFile.exists()){
        resultFile.mkdir()
      }

      resultFile
  }

  // This is weird no? Shouldn't I have configurable period to look back.
  val previousConflicts = s"$resultDirectory/$TWODAYS_AGO"

  implicit object PipeFormat extends DefaultCSVFormat {
    override val delimiter = '|'
  }

  def parseFileAsRecords(source: String): Records = {

    // What to do with header and summaries, do we want to check integrity?

    using(CSVReader.open(source)) { reader =>
      val lines: List[List[String]] = reader.all()
        .filter(!_.head.startsWith("#"))
          .drop(4)
      // FixME: Dropping header and summaries, for now.
      // Better way is to parse it and verify/report error if summary does not match with the following records.

      val records = lines.map(Record.apply)

      val (asn, ipv4, ipv6) = records.foldLeft((List[AsnRecord](), List[Ipv4Record](), List[Ipv6Record]())){
        case ((curAsn, curIpv4, curIpv6), nextRecord) => nextRecord match {
          case a : AsnRecord => (a :: curAsn, curIpv4, curIpv6)
          case b : Ipv4Record => (curAsn, b::curIpv4, curIpv6)
          case c : Ipv6Record => (curAsn, curIpv4, c::curIpv6)
        }
      }

      Records(asn, ipv4, ipv6)
    }
  }

  // Fetch only if data file is not yet downloaded.
  def fetchLocally(source: String, dest: String) {
    if (new File(dest).isFile) {
      System.err.println(s"     File $dest exist, not fetching")
      return
    }
    System.err.println(s"---Fetching $source into $dest---")
    val response = requests.get(source)

    using(new PrintWriter(new File(dest))) { writer =>
      writer.write(response.text())
      System.err.println(s"---Done fetching $source into $dest---\n\n\n")
    }
  }

  // Assumes a data directory existed to store fetched data.
  def fetchAndParse(): (Iterable[Records], Records) = {
    val recordMaps = dataSources.map {
      case (name, url) =>
        fetchLocally(url, s"$dataDirectory/$name")
        (name, parseFileAsRecords(s"$dataDirectory/$name"))
    }
    // Adjusting and fixing record fields conforming to what is done by geoff.
    val rirs = (recordMaps - "iana" - "geoff").mapValues(_.fixRIRs).values.seq
    val iana = recordMaps("iana").fixIana
    (rirs, iana)
  }

  def writeResult(asn: ListRecords, ipv4: ListRecords, ipv6: ListRecords, outputFile: String = s"$resultToday/combined-stat") {
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

  def writeConflicts(conflicts: List[Conflict], outputFile: String = s"$resultToday/conflicts"): Unit = {
    using( CSVWriter.open(new File(outputFile))) { writer =>
      writer.writeAll(conflicts.map(_.asList))
    }
  }

  def readConflicts(conflictFile: String): immutable.Seq[Conflict] = {
    using(CSVReader.open(new File(conflictFile))){ reader =>
        reader.all().map(Conflict.apply)
    }
  }

  def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A =
    try { f(closeable) } finally { closeable.close() }

}
