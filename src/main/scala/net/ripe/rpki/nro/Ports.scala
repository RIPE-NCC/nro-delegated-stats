package net.ripe.rpki.nro

import java.io.{File, PrintWriter}

import net.ripe.ipresource.IpResource
import net.ripe.rpki.nro.Defs._

import scala.collection.SortedMap
import scala.io.Source.fromFile

// Importing data from remotes files and exporting to file.
// Or maybe Ports from Port & Adapter/Hexagonal architecture, i.e stuff on the edge.
object Ports {

  def parseLines(lines: List[String]): List[Line] = lines.map(_.split('|'))


  def parseFileAsRecords(source: String): Records = {

    using(fromFile(source)) { src =>
      val lines = src.getLines.filter(!_.startsWith("#")).toList
      val header = lines.head.split('|')
      val summaries = parseLines(lines.filter(_.contains(SUMMARY)))

      val recordLines = lines.tail.filter(!_.contains(SUMMARY))

      // Parse and create sorted map from range to record
      val asn  = parseLines(recordLines.filter(_.contains(ASN)) ).map(AsnRecord .apply)
      val ipv4 = parseLines(recordLines.filter(_.contains(IPV4))).map(Ipv4Record.apply)
      val ipv6 = parseLines(recordLines.filter(_.contains(IPV6))).map(Ipv6Record.apply)

      Records(source, header, summaries, asn, ipv4, ipv6)
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
        fetchLocally(url, s"data/$name")
        (name, parseFileAsRecords(s"data/$name"))
    }
    // Adjusting and fixing record fields conforming to what is done by geoff.
    val rirs = (recordMaps - "iana" - "geoff").mapValues(_.fixRIRs).values.seq
    val iana = recordMaps("iana").fixIana
    (rirs, iana)
  }

  def writeResult(asn: ListRecords, ipv4: ListRecords, ipv6: ListRecords, outputFile: String = "result/combined-stat") {
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

  def writeConflicts(conflicts: List[Conflict], outputFile: String = "result/conflicts"): Unit = {
    using(new PrintWriter(new File(outputFile))) { writer =>
      writer.write(conflicts.mkString("\n"))
      if(conflicts.nonEmpty){
        println("Conflicts found:")
        println(conflicts.mkString("\n"))
      }
    }
  }
  def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A =
    try { f(closeable) } finally { closeable.close() }

}
