package net.ripe.rpki.nro.main

import java.io.StringReader

import com.github.tototoshi.csv.CSVReader
import net.ripe.commons.ip.{Ipv4, Ipv4Range, Ipv6Range}
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.model.{Record, Records}
import net.ripe.rpki.nro.service.Ports

import scala.util.{Try, Using}

object IanaMagic extends  Merger with Logging{
  val RIRs = List("arin", "ripe", "apnic", "lacnic", "afrinic")
  val rangeRex = """(\d+)-(\d+)""".r
  val whoisRex = """whois.(\w+).net""".r
  val v4slash8 = """(\d\d\d)/8""".r
  val v4Rex = """(\d+).(\d+).(\d+).(\d+)/(\d+)""".r
  val v6Rex = """(.*)::/(\d+)""".r
  val YYYYMM = """(\d\d\d\d)-(\d\d)""".r

  def parseAsnRecord(asnRecord: List[String]): List[String] = asnRecord match {
    case asNum :: _ :: whois :: _ :: _ :: date :: _ => asnRange(asNum) :+ resolveDate(date) :+ whoisRIR(whois)
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

  def parseIpv6Records(ipv6Source: String) = {
    val ipv6Text = requests.get(ipv6Source).text()
    // Skipping header here.
    readCSV(ipv6Text).tail.map(buildIpv6Record)
  }

  def parseIpv4Recovered(ipv4Source: String) = {
    val ipv4Text = requests.get(ipv4Source).text()
    // Skipping header here.
    readCSV(ipv4Text).tail.map(buildIpv4RecoveredRecord)
  }

  def parseIpv4Special(ipv4Source: String) = {
    val ipv4Text = requests.get(ipv4Source).text()
    // Special treatment for special registry.
    // Giving up trying to parse 192.0.0.0/24 [2] or 192.0.0.170/32, 192.0.0.171/32 where the rest are just normal ranges.
    readCSV(ipv4Text).tail.flatMap(line => Try(buildIpv4SpecialRecord(line)).toOption)
  }

  def buildIpv4Record(ipv4Record: List[String]): List[String] = ipv4Record match {
    case range :: _ :: date :: whois :: _ =>
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(range) ++ List(resolveDate(date), whoisRIR(whois))
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }

  def buildIpv6Record(ipv6Record: List[String]): List[String] = ipv6Record match {
    case range :: _ :: date :: whois :: _ =>
      List("iana", "ZZ", "ipv6") ++ toPrefixLength(range) ++ List(resolveDate(date), whoisRIR(whois))
    case _ => throw new Exception(s"Can't parse this line: $ipv6Record")
  }

  def resolveDate(date: String) = {
    val unformatted =
      if (date.contains("RFC")) {
        "19960801"
      } else if (date.isEmpty) {
        "19920101"
      } else {
        date
      }
    unformatted match {
      case YYYYMM(year, month) => s"$year${month}01"
      case _ => unformatted.replaceAll("-", "")
    }

  }

  def toPrefixLength(range: String): List[String] = range match {
    case v4slash8(last8bit) => {
      val r = Ipv4Range.parse(s"$last8bit.0.0.0/8")
      List(s"${r.start()}", s"${r.size()}")
    }
    case v6Rex(_, length) => {
      val r = Ipv6Range.parse(range)
      List(s"${r.start().toString}", length)
    }
    case v4Rex(a,b,c,d,_) => {
      val r = Ipv4Range.parse(range)
      List(s"${r.start()}", s"${r.size()}")
    }
    case _ => throw new Exception(s"Can't parse this line: $range")
  }

  def buildIpv4RecoveredRecord(ipv4Record: List[String]): List[String] = ipv4Record match {
    case prefixStart :: prefixEnd :: rir :: date :: _ =>
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(prefixStart, prefixEnd) ++ List(resolveDate(date), s"${rir.toLowerCase.replaceAll(" ", "")}")
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }
  def buildIpv4SpecialRecord(ipv4Record: List[String]): List[String] = ipv4Record match {
    case range :: _ :: _ :: date :: _ =>
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(range) ++ List(resolveDate(date), "ietf")
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }

  def toPrefixLength(prefixStart: String, prefixEnd: String): List[String] = {
    val start = Ipv4.parse(prefixStart)
    val end = Ipv4.parse(prefixEnd)
    val range = Ipv4Range.from(start).to(end)
    List(s"$start", s"${range.size()}")
  }


   def fetchIanaRecords: Records = {
    logger.info("Fetch ASN16")
    val asn16 = parseAsnRecords("https://www.iana.org/assignments/as-numbers/as-numbers-1.csv")
    // Careful, IANA 32 contains this entry for 16 that needs to be skipped: iana|ZZ|asn|0|65536
     logger.info("Fetch ASN32")
    val asn32 = parseAsnRecords("https://www.iana.org/assignments/as-numbers/as-numbers-2.csv").tail
     logger.info("Fetch ipv4 address space")
    val ipv4 = parseIpv4Records("https://www.iana.org/assignments/ipv4-address-space/ipv4-address-space.csv")
     logger.info("Fetch ipv6 address space")
    val ipv6 = parseIpv6Records("https://www.iana.org/assignments/ipv6-address-space/ipv6-address-space-1.csv")
     logger.info("Fetch ipv6 unicast space")
    val ipv6unicast = parseIpv6Records("https://www.iana.org/assignments/ipv6-unicast-address-assignments/ipv6-unicast-address-assignments.csv")


    val originalIana = Ports.toRecords(asn16 ++ asn32 ++ ipv4 ++ ipv6).fixIana

    val ipv4Recovered1 = parseIpv4Recovered("https://www.iana.org/assignments/ipv4-recovered-address-space/ipv4-recovered-address-space-1.csv")
    val ipv4Recovered2 = parseIpv4Recovered("https://www.iana.org/assignments/ipv4-recovered-address-space/ipv4-recovered-address-space-2.csv")
    val ipv4SpecialReg = parseIpv4Special("https://www.iana.org/assignments/iana-ipv4-special-registry/iana-ipv4-special-registry-1.csv")

    val recoverReserved = Ports.toRecords(ipv4Recovered1 ++ ipv4Recovered2 ++ ipv4SpecialReg ++ ipv6unicast).fixIana

    originalIana.substract(recoverReserved).append(recoverReserved).sorted()
  }

  val test = fetchIanaRecords

  def reformat(rec: Record) = rec.asList.init.init.mkString("|")
  test.asn.foreach(r => println(reformat(r)))
  test.ipv4.foreach(r => println(reformat(r)))
  test.ipv6.foreach(r => println(reformat(r)))


  def readCSV(str: String): List[List[String]] = Using.resource(CSVReader.open(new StringReader(str)))(_.all).filter(_.nonEmpty)

  //TODO: Deal with extra e-stats, and check if it will affect the rest.
}