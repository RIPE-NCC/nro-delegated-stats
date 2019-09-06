package net.ripe.rpki.nro

import java.io._
import java.math.BigInteger

import net.ripe.commons.ip.Ipv6Range
import net.ripe.ipresource._

import scala.collection.JavaConverters._
import scala.collection.SortedMap
import scala.io.Source
import Defs._

object Main extends App {

  implicit val ipResourceRangeOrder = new Ordering[IpResourceRange] {
    override def compare(a: IpResourceRange, b: IpResourceRange) = a.compareTo(b)
  }

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
      parse(records.filter(_.contains("asn"))).map(a => AsnRecord(a)).map(r => r.range() -> r): _*)
    val ipv4 = SortedMap(
      parse(records.filter(_.contains("ipv4")))
        .map(r => Ipv4Record(r))
        .map(r => r.range() -> r): _*)
    val ipv6 = SortedMap(
      parse(records.filter(_.contains("ipv6")))
        .map(r => Ipv6Record(r))
        .map(r => r.range() -> r): _*)

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
  def loadAll() = {

    val records: Map[String, Records] = dataSources.map {
      case (name, url) => {
        fetch(url, s"data/$name")
        (name, parseRecords(s"data/$name"))
      }
    }

    val rirs = (records - "iana" - "jeff").mapValues(_.fixExt.fixZZDate.fixAllocated)
    val iana = records("iana").fixExt.fixOid
    val jeff = records("jeff")

    (rirs, iana, jeff)
  }

  // I guess we can do conflict detection here.
  def merge(ma: SortedMap[IpResourceRange, Record], mb: SortedMap[IpResourceRange, Record]) = {
    val ka       = ma.keySet
    val kb       = mb.keySet
    val conflict = ka.intersect(kb)
    conflict.foreach(k => {
      val va = ma(k)
      val vb = mb(k)

      println(s"$k is found both on ${va.registry} and ${vb.registry}")
      println(s"<    $va ")
      println(s">    $vb ")
    })
    ma ++ mb
  }

  // Combining multiple maps from different RIRs
  def combine(resourceMap: Iterable[SortedMap[IpResourceRange, Record]]) =
    resourceMap.foldLeft(SortedMap[IpResourceRange, Record]())(merge)

  def rangeLen(r: IpResourceRange) =
    r.getEnd.getValue.subtract(r.getStart.getValue).add(BigInteger.ONE)

  def asnPool(asn: IpResourceRange) =
    AsnRecord("iana",
              "ZZ",
              "asn",
              asn.getStart.getValue + "",
              rangeLen(asn) + "",
              TODAY,
              "ianapool",
              "",
              "iana")

  // Ipv4 ianapool on Jeff's combined results always dated 20120801, Magic date, where is it from? The ASN and Ipv6 is dated TODAY
  def ipv4Pool(ipv4: IpResourceRange) =
    Ipv4Record("iana",
               "ZZ",
               "ipv4",
               ipv4.getStart + "",
               rangeLen(ipv4) + "",
               "20120801",
               "ianapool",
               "",
               "iana")

  def ipv6Pool(ipv6: IpResourceRange) = {
    val Array(start, prefix) = ipv6.toString.split("/")
    Ipv6Record("iana", "ZZ", "ipv6", start, prefix, TODAY, "ianapool", "", "iana")
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

  def combineAll() = {
    val (rirs, iana, jeff) = loadAll

    // IETF reserved data from IANA is combined without modification
    val asnIetf  = iana.asn.filter { case (_, v)  => v.status == "ietf" || v.status == "assigned" }
    val ipv4Ietf = iana.ipv4.filter { case (_, v) => v.status == "ietf" || v.status == "assigned" }
    val ipv6Ietf = iana.ipv6.filter { case (_, v) => v.status == "ietf" || v.status == "assigned" }

    // Combine will also check for conflicts inter RIRS, not checking integrity within RIR
    println(s"\n\n---  Combining RIRs data and checking for conflicts ---\n\n")
    val asns  = combine(rirs.values.map(_.asn)) ++ asnIetf
    val ipv4s = combine(rirs.values.map(_.ipv4)) ++ ipv4Ietf
    val ipv6s = combine(rirs.values.map(_.ipv6)) ++ ipv6Ietf

    // Started with slash zero and remove one by one all ipv4s we found by combining RIRs, we ended up with ianapool
    val ianapool4 = new IpResourceSet
    ianapool4.add(IpResourceRange.parse("0.0.0.0/0"))
    ipv4s.keys.foreach(ianapool4.remove)

    // Ipv4 does not have to fit in bit boundary no need for prefix splitting.
    val ipv4sp = ipv4s ++ ianapool4.iterator.asScala
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> ipv4Pool(a))
      .toMap

    // Started with slash zero and remove one by one all ipv6s we found by combining RIRs, we ended up with ianapool
    val ianapool6 = new IpResourceSet
    ianapool6.add(IpResourceRange.parse("::/0"))
    ipv6s.keys.foreach(ianapool6.remove)

    // IPv6 has to be fit into bit boundary, since the format is | start | prefixLength
    val ipv6sp = ipv6s ++ ianapool6.iterator.asScala
      .map(_.toString)
      .flatMap(s => Ipv6Range.parse(s).splitToPrefixes.asScala)
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> ipv6Pool(a))
      .toMap

    val ianapoolAsn = new IpResourceSet
    ianapoolAsn.add(IpResourceRange.parse("AS0-AS4200000000"))
    asns.keys.foreach(ianapoolAsn.remove)
    val asnsp = asns ++ ianapoolAsn.iterator.asScala
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> asnPool(a))
      .toMap

    writeOut(asnsp.values.toList, ipv4sp.values.toList, ipv6sp.values.toList)
    ((rirs, iana, jeff), (asnsp, ipv4sp, ipv6sp))

  }

  combineAll()
  println("Done")

}
