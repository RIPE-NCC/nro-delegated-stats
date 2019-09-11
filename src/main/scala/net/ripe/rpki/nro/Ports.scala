package net.ripe.rpki.nro


import java.io.{File, PrintWriter}

import net.ripe.ipresource.IpResource
import net.ripe.rpki.nro.Defs._

import scala.collection.SortedMap
import scala.collection.parallel.immutable.ParMap
import scala.io.Source.fromFile

// Importing data from remotes files and exporting to file.
// Or maybe Ports from Port & Adapter/Hexagonal architecture, i.e stuff on the edge.
object Ports {

  val dataSources: Map[String, String] = Map[String, String](
    APNIC   -> "http://ftp.apnic.net/stats/apnic/delegated-apnic-extended-latest",
    AFRINIC -> "http://ftp.afrinic.net/stats/afrinic/delegated-afrinic-extended-latest",
    ARIN    -> "http://ftp.arin.net/pub/stats/arin/delegated-arin-extended-latest",
    LACNIC  -> "http://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-extended-latest",
    RIPENCC -> "https://ftp.ripe.net/pub/stats/ripencc/delegated-ripencc-extended-latest",
    IANA    -> "http://ftp.apnic.net/pub/stats/iana/delegated-iana-latest",
    JEFF    -> "https://www.nro.net/wp-content/uploads/apnic-uploads/delegated-extended"
  )

  def parseLines(lines: List[String]): List[Line] = lines.map(_.split('|'))

  def toSortedRecordMap[R <: Record](lines : List[Line], f : Line => R) : SortedMap[IpResource, R] = {
    SortedMap(lines.map(r => f(r)).map(r => r.range() -> r): _*)
  }

  def parseFile(source: String): Records = {

    using(fromFile(source)) { src =>
      val lines = src.getLines.filter(!_.startsWith("#")).toList
      val header = lines.head.split('|')
      val summaries = parseLines(lines.filter(_.contains(SUMMARY)))

      val recordLines = lines.tail.filter(!_.contains(SUMMARY))

      // Parse and create sorted map from range to record
      val asn  = toSortedRecordMap(parseLines(recordLines.filter(_.contains(ASN))), AsnRecord.apply)
      val ipv4 = toSortedRecordMap(parseLines(recordLines.filter(_.contains(IPV4))), Ipv4Record.apply)
      val ipv6 = toSortedRecordMap(parseLines(recordLines.filter(_.contains(IPV6))), Ipv6Record.apply)

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
  def fetchAndParse(): ParMap[String, Records] = {
    dataSources.par.map {
      case (name, url) =>
        fetchLocally(url, s"data/$name")
        (name, parseFile(s"data/$name"))
    }
  }

  def writeCombined(asn: SortedRecordsMap, ipv4: SortedRecordsMap, ipv6: SortedRecordsMap) {
    using(new PrintWriter(new File("result/combined-stat"))) { writer =>

      val totalSize = asn.size + ipv4.size + ipv6.size
      val SERIAL = 19821213

      writer.write(s"2|nro|$TODAY|$totalSize|$SERIAL|$TODAY|+0000\n")
      writer.write(s"nro|*|asn|*|${asn.size}|summary\n")
      writer.write(s"nro|*|ipv4|*|${ipv4.size}|summary\n")
      writer.write(s"nro|*|ipv6|*|${ipv6.size}|summary\n")
      writer.write(asn.values.map(_.toString).mkString("\n"))
      writer.write("\n")
      writer.write(ipv4.values.map(_.toString).mkString("\n"))
      writer.write("\n")
      writer.write(ipv6.values.map(_.toString).mkString("\n"))
    }
  }

  def writeConflicts(conflicts: List[Conflict]): Unit = {
    using(new PrintWriter(new File("result/conflicts"))) { writer =>
      writer.write(conflicts.mkString("\n"))
      if(!conflicts.isEmpty){
        println("Conflicts found:")
        println(conflicts.mkString("\n"))
      }
    }
  }
  def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A =
    try { f(closeable) } finally { closeable.close() }

}
