package net.ripe.rpki.nro.iana
import java.io.StringReader

import com.github.tototoshi.csv.CSVReader
import net.ripe.commons.ip.{Ipv4, Ipv4Range, Ipv6Range, PrefixUtils}
import net.ripe.rpki.nro.iana.IanaGenerator.logger

import scala.util.matching.Regex
import scala.util.{Try, Using}

trait IanaParser {

  val RIRs = List("arin", "ripe", "apnic", "lacnic", "afrinic")

  private def fetchAndParse(source: String, parser: List[String] => List[String], headerSkip: Int = 1) = {
    val text = requests.get(source).text()
    val skipHeader = text.split("\n").toList.drop(headerSkip).mkString("\n")
    readCSV(skipHeader).map(parser).filter(_.nonEmpty)
  }

  def fetchAsn(asnSource: String): Seq[List[String]]    = fetchAndParse(asnSource, parseAsnLine, headerSkip = 2)
  def fetchIpv4(ipv4Source: String): Seq[List[String]]  = fetchAndParse(ipv4Source, parseIpv4Line)
  def fetchIpv6(ipv6Source: String): Seq[List[String]]  = fetchAndParse(ipv6Source, parseIpv6Line)

  def fetchIpv4Recovered(ipv4Source: String): Seq[List[String]]    = fetchAndParse(ipv4Source, parseIpv4RecoveredLine)
  def fetchIpv4Reallocated(ipv4Source: String): Seq[List[String]]  = fetchAndParse(ipv4Source, parseIpv4ReallocatedLine)
  def fetchIpv4SpecialRegs(ipv4Source: String): Seq[List[String]]  = fetchAndParse(ipv4Source, parseIpv4SpecialLine)

  def parseAsnLine(asnRecord: List[String]): List[String] = asnRecord match {
    case asNum :: "Unallocated" :: _ => asnRange(asNum) :+ "20061129" :+ "iana"
    case asNum :: _ :: whois :: _ :: _ :: date :: _ => asnRange(asNum) :+ resolveDate(date) :+ whoisRIR(whois)
    case _ => throw new Exception(s"Can't parse this line: $asnRecord")
  }

  def parseIpv4Line(ipv4Record: List[String]): List[String] = ipv4Record match {
    case range :: _ :: date :: whois :: _ => List("iana", "ZZ", "ipv4") ++ toPrefixLength(range) :+ resolveDate(date) :+ whoisRIR(whois)
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }

  def parseIpv6Line(ipv6Record: List[String]): List[String] = ipv6Record match {
    // Skip reserved
    case _ :: _ :: _ :: _ :: _ :: "RESERVED" :: _ => List()
    case range :: _ :: date :: whois :: _ => List("iana", "ZZ", "ipv6") ++ toPrefixLength(range) :+ resolveDate(date) :+ whoisRIR(whois)
    case _ => logger.error(s"Can't parse this line: $ipv6Record"); List()
  }

  def parseIpv4RecoveredLine(ipv4Record: List[String]): List[String] = ipv4Record match {
    case prefixStart :: prefixEnd :: _ :: date :: _ =>
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(prefixStart, prefixEnd) :+ resolveDate(date) :+ "iana"
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }

  def parseIpv4ReallocatedLine(ipv4Record: List[String]): List[String] = ipv4Record match {
    case prefixStart :: prefixEnd :: rir :: date :: _ =>
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(prefixStart, prefixEnd) :+ resolveDate(date) :+ s"${rir.toLowerCase.replaceAll(" ", "")}"
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }

  val specialIana = Set("192.31.196.0/24", "192.52.193.0/24")

  def parseIpv4SpecialLine(ipv4SpecialRegistries: List[String]): List[String] = ipv4SpecialRegistries match {

    // Self conflicting and already marked as future use in ipv4-address-space, so skip.
    case "240.0.0.0/4" :: _ => List()
    case "255.255.255.255/32" :: _ => List()

    // Special registry which is claimed by ARIN and not included in current IANA.
    case "192.175.48.0/24" :: _ => List()

    // Can't parse this [reference], so special treatment.
    case "192.0.0.0/24 [2]" :: _ :: _ :: date :: _ => List("iana", "ZZ", "ipv4") ++ toPrefixLength("192.0.0.0/24") :+ resolveDate(date) :+ "ietf"

    // Self conflicting small ranges are skipped, they are subset of others.
    case range :: _ if range.endsWith("/32") || range.endsWith("/29") => List()

    case range :: _ :: _ :: date :: _ =>
      val ianaIetf = if (specialIana.contains(range)) {
        "iana"
      } else {
        "ietf"
      }
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(range) ++ List(resolveDate(date), ianaIetf)

    case _ => logger.error(s"Can't parse this line: $ipv4SpecialRegistries"); List()
  }

  private def resolveDate(date: String) = {
    val unformatted =
      if (date.contains("RFC")) {
        "19960801"
      } else if (date.isEmpty) {
        "19920101"
      } else {
        date
      }
    unformatted match {
      case YYYYMMRegex(year, month) => s"$year${month}01"
      case _ => unformatted.replaceAll("-", "")
    }

  }

  def toPrefixLength(range: String): List[String] = range match {
    case v4slash8Regex(last8bit) =>
      val r = Ipv4Range.parse(s"$last8bit.0.0.0/8")
      List(s"${r.start()}", s"${r.size()}")

    case _ =>
      val r = Try(Ipv6Range.parse(range)).toOption
      if (r.isDefined) {
        List(s"${r.get.start().toString}", PrefixUtils.getPrefixLength(r.get).toString)
      }
      else {
        val rr = Try(Ipv4Range.parse(range)).toOption
        if (rr.isDefined) {
          List(s"${rr.get.start()}", s"${rr.get.size()}")
        } else {
          throw new Exception(s"Can't parse this line: $range")
        }
      }
  }

  private def toPrefixLength(prefixStart: String, prefixEnd: String): List[String] = {
    val start = Ipv4.parse(prefixStart)
    val end = Ipv4.parse(prefixEnd)
    val range = Ipv4Range.from(start).to(end)
    List(s"$start", s"${range.size()}")
  }

  private val rangeRegex: Regex    = """(\d+)-(\d+)""".r
  private val whoisRegex: Regex    = """whois.(\w+).net""".r
  private val v4slash8Regex: Regex = """(\d\d\d)/8""".r
  private val YYYYMMRegex: Regex   = """(\d\d\d\d)-(\d\d)""".r

  private def asnRange(asNum: String): List[String] = asNum match {
    case rangeRegex(start, end) => List("iana", "ZZ", "asn", start, s"${end.toLong - start.toLong + 1}")
    case _ => List("iana", "ZZ", "asn", s"${asNum.toLong}", "1")
  }

  private def whoisRIR(whois: String): String = whois match {
    case whoisRegex(rir) => if (rir == "ripe") "ripencc" else rir
    case _ => "ietf"
  }

  private def readCSV(str: String): List[List[String]] =
    Using.resource(CSVReader.open(new StringReader(str)))(_.all().filter(_.nonEmpty))
}
