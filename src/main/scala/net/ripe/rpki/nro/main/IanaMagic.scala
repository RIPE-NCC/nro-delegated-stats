package net.ripe.rpki.nro.main

import java.io.StringReader

import com.github.tototoshi.csv.CSVReader
import net.ripe.commons.ip.{Ipv4, Ipv4Range}
import net.ripe.rpki.nro.service.Ports

import scala.util.Using

object IanaMagic extends App with Merger {
  val RIRs = List("arin", "ripe", "apnic", "lacnic", "afrinic")
  val rangeRex = """(\d+)-(\d+)""".r
  val whoisRex = """whois.(\w+).net""".r
  val slashEightRex = """(\d\d\d)/8""".r


  def parseAsnRecord(asnRecord: List[String]): List[String] = asnRecord match {
    case asNum :: _ :: whois :: _ :: _ :: date :: _ => asnRange(asNum) :+ date :+ whoisRIR(whois)
    case _ => throw new Exception(s"Can't parse this line: $asnRecord")
  }

  def asnRange(asNum: String): List[String] = asNum match {
    case rangeRex(start, end) => List("iana", "ZZ", "asn", start, s"${end.toLong - start.toLong + 1}")
    case _ => List("iana", "ZZ", "asn", s"${asNum.toLong}", "1")
  }

  def whoisRIR(whois: String): String = whois match {
    case whoisRex(rir) => if (rir == "ripe") {
      "ripencc"
    } else {
      rir
    }
    case _ => "ietf"
  }

  def parseAsnRecords(asnSource: String) = {
    val asnText = requests.get(asnSource).text()
    // skipping weird header, skipping first two lines.
    val asnNoHeader = asnText.split("\n").tail.tail.mkString("\n")

    readCSV(asnNoHeader).map(parseAsnRecord)
  }

  def parseIpv4Records(ipv4Source: String) = {
    val ipv4Text = requests.get(ipv4Source).text()
    // Skipping header here.
    readCSV(ipv4Text).tail.map(buildIpv4Record)
  }

  def parseIpv4Recovered(ipv4Source: String) = {
    val ipv4Text = requests.get(ipv4Source).text()
    // Skipping header here.
    readCSV(ipv4Text).tail.map(buildIpv4RecoveredRecord)
  }

  def buildIpv4Record(ipv4Record: List[String]): List[String] = ipv4Record match {
    case range :: _ :: date :: whois :: _ =>
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(range) ++ List(date, whoisRIR(whois))
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }

  def toPrefixLength(range: String): List[String] = range match {
    case slashEightRex(last8bit) => {
      val r = Ipv4Range.parse(s"$last8bit.0.0.0/8")
      List(s"${r.start()}", s"${r.size()}")
    }
  }

  def buildIpv4RecoveredRecord(ipv4Record: List[String]): List[String] = ipv4Record match {
    case prefixStart :: prefixEnd :: rir :: date :: _ =>
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(prefixStart, prefixEnd) ++ List(date, s"${rir.toLowerCase.replaceAll(" ", "")}")
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }

  def toPrefixLength(prefixStart: String, prefixEnd: String): List[String] = {
    val start = Ipv4.parse(prefixStart)
    val end = Ipv4.parse(prefixEnd)
    val range = Ipv4Range.from(start).to(end)
    List(s"$start", s"${range.size()}")
  }


  val asn16 = parseAsnRecords("https://www.iana.org/assignments/as-numbers/as-numbers-1.csv")
  // Careful, IANA 32 contains this entry for 16 that needs to be skipped: iana|ZZ|asn|0|65536
  val asn32 = parseAsnRecords("https://www.iana.org/assignments/as-numbers/as-numbers-2.csv").tail
  val ipv4 = parseIpv4Records("https://www.iana.org/assignments/ipv4-address-space/ipv4-address-space.csv")

  val originalIana = Ports.toRecords(asn16 ++ asn32 ++ ipv4).fixIana

  val ipv4Recovered1 = parseIpv4Recovered("https://www.iana.org/assignments/ipv4-recovered-address-space/ipv4-recovered-address-space-1.csv")
  val ipv4Recovered2 = parseIpv4Recovered("https://www.iana.org/assignments/ipv4-recovered-address-space/ipv4-recovered-address-space-2.csv")

  val recoveredIana = Ports.toRecords(ipv4Recovered1 ++ ipv4Recovered2).fixIana

  val test = originalIana.substract(recoveredIana).append(recoveredIana)
  test.asn.sorted.foreach(println)
  test.ipv4.sorted.foreach(println)


  def readCSV(str: String): List[List[String]] = Using.resource(CSVReader.open(new StringReader(str)))(_.all).filter(_.nonEmpty)
}