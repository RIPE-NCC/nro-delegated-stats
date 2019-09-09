package net.ripe.rpki.nro

import java.io._
import scala.io._

import scala.collection._
import scala.collection.JavaConverters._

import Defs._

object Ports {

  val dataSources = Map[String, String](
    "apnic"   -> "http://ftp.apnic.net/stats/apnic/delegated-apnic-extended-latest",
    "afrinic" -> "http://ftp.afrinic.net/stats/afrinic/delegated-afrinic-extended-latest",
    "arin"    -> "http://ftp.arin.net/pub/stats/arin/delegated-arin-extended-latest",
    "lacnic"  -> "http://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-extended-latest",
    "ripencc" -> "https://ftp.ripe.net/pub/stats/ripencc/delegated-ripencc-extended-latest",
    "iana"    -> "http://ftp.apnic.net/pub/stats/iana/delegated-iana-latest",
    "jeff"    -> "https://www.nro.net/wp-content/uploads/apnic-uploads/delegated-extended"
  )

  def parseRecords(source: String): Records = {

    def parse(records: List[String]): List[Line] = records.map(_.split('|'))

    val lines     = Source.fromFile(source).getLines.filter(!_.startsWith("#")).toList
    val header    = lines.head.split('|')
    val summaries = parse(lines.filter(_.contains("summary")))

    val records = lines.tail.filter(!_.contains("summary"))

    // Parse and create sorted map from range to record
    val asn = SortedMap(
      parse(records.filter(_.contains("asn"))).map(a => AsnRecord(a)).map(r => r.range() -> r): _*
    )
    val ipv4 = SortedMap(
      parse(records.filter(_.contains("ipv4")))
        .map(r => Ipv4Record(r))
        .map(r => r.range() -> r): _*
    )
    val ipv6 = SortedMap(
      parse(records.filter(_.contains("ipv6")))
        .map(r => Ipv6Record(r))
        .map(r => r.range() -> r): _*
    )

    Records(source, header, summaries, asn, ipv4, ipv6)
  }

  // Fetch only if data file is not yet downloaded.
  def fetch(source: String, dest: String) {
    if (new File(dest).isFile) {
      System.err.println(s"     File $dest exist, not fetching")
      return
    }
    System.err.println(s"---Fetching $source into $dest---")
    val response = requests.get(source)
    val writer   = new PrintWriter(new File(dest))
    writer.write(response.text())
    writer.close()
    System.err.println(s"---Done fetching $source into $dest---\n\n\n")
  }

  // Assumes a data directory existed to store fetched data.
  def fetchAndParse() = {

    val records: Map[String, Records] = dataSources.map {
      case (name, url) => {
        fetch(url, s"data/$name")
        (name, parseRecords(s"data/$name"))
      }
    }

    val rirs = (records - "iana" - "jeff").mapValues(_.fixExt.fixReservedAvailable.fixAllocated)
    val iana = records("iana").fixExt.fixIetfIana
    val jeff = records("jeff")

    (rirs, iana, jeff)
  }

  def writeOut(asn: List[Record], ipv4: List[Record], ipv6: List[Record]) {
    val writer    = new PrintWriter(new File("result/combined-stat"))
    val totalSize = asn.size + ipv4.size + ipv6.size
    val SERIAL    = 19821213

    writer.write(s"2|nro|$TODAY|$totalSize|$SERIAL|$TODAY|+0000\n")
    writer.write(s"nro|*|asn|*|${asn.size}|summary\n")
    writer.write(s"nro|*|ipv4|*|${ipv4.size}|summary\n")
    writer.write(s"nro|*|ipv6|*|${ipv6.size}|summary\n")
    writer.write(asn.map(_.toString).mkString("\n"))
    writer.write("\n")
    writer.write(ipv4.map(_.toString).mkString("\n"))
    writer.write("\n")
    writer.write(ipv6.map(_.toString).mkString("\n"))
    writer.close

  }

}
