package net.ripe.rpki.nro.iana

import java.io.StringReader

import com.github.tototoshi.csv.CSVReader
import net.ripe.commons.ip.{Ipv4, Ipv4Range, Ipv6Range, PrefixUtils}
import net.ripe.rpki.nro.Configs.ianaorg
import net.ripe.rpki.nro.Const.{ASN16, ASN32, IPV4_ADDRESS_SPACE, IPV4_REALLOCATED_SPACE, IPV4_RECOVERED_SPACE, IPV4_SPECIAL_REGISTRY, IPV6_ADDRESS_SPACE, IPV6_UNICAST_ASSIGNMENT}
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.main.Merger
import net.ripe.rpki.nro.model.Records
import net.ripe.rpki.nro.service.Ports

import scala.util.{Try, Using}

object IanaMagic extends Merger with Logging {

  def processIanaRecords: Records = {

    val originalIanaSpace: Records = fetchAllIanaSpace().substract(unicastV6AndRecoveredV4exclusion()).fixIana
    val reallocatedAssigned: Records = fetchUnicastAssignmentV6ReallocatedSpecialV4()

    val (ianaMagic, _) = combineRecords(Seq(originalIanaSpace, reallocatedAssigned), Some(reallocatedAssigned))
    Ports.writeRecords(ianaMagic, "iana-own-magic")

    ianaMagic
  }

  def fetchAllIanaSpace(): Records = {
    logger.info("Fetch ASN16")
    val asn16 = parseAsnRecords(ianaorg(ASN16))

    logger.info("Fetch ASN32")
    // Careful, IANA 32 contains this entry for 16 that needs to be skipped: iana|ZZ|asn|0|65536 => Explains the tail
    val asn32 = parseAsnRecords(ianaorg(ASN32)).tail

    logger.info("Fetch ipv4 address space")
    val ipv4 = parseIpv4Records(ianaorg(IPV4_ADDRESS_SPACE))

    logger.info("Fetch ipv6 address space")
    val ipv6 = parseIpv6Records(ianaorg(IPV6_ADDRESS_SPACE))

    Ports.toRecords(asn16 ++ asn32 ++ ipv4 ++ ipv6).fixIana
  }

  private def unicastV6AndRecoveredV4exclusion() = {
    // Exclude all global unicast, but include the first /16, don't really have explanation why.
    val includefirst16 = Ports.toRecords(List(List("iana", "ZZ", "ipv6") ++ toPrefixLength("2000::/16") ++ List("19960801", "ietf")))

    // Whole global unicast 2000::/33 minus 2000::/16 since we want to include the latter.
    val unicastV6 = Ports.toRecords(List(List("iana", "ZZ", "ipv6") ++ toPrefixLength("2000::/3") ++ List("1990", "ietf"))).substract(includefirst16)

    // Recovered but not allocated
    val ipv4Recovered = Ports.toRecords(parseIpv4Reallocated(ianaorg(IPV4_RECOVERED_SPACE)))

    unicastV6.append(ipv4Recovered)
  }

  def fetchUnicastAssignmentV6ReallocatedSpecialV4(): Records = {

    // Recovered and reallocated, we need this.
    val ipv4Reallocated = parseIpv4Reallocated(ianaorg(IPV4_REALLOCATED_SPACE))

    // Somehow not sure if needed but with some special 'care' we can include.
    val ipv4SpecialRegistry = parseIpv4Special(ianaorg(IPV4_SPECIAL_REGISTRY))

    logger.info("Fetch ipv6 unicast space, returning only those for RIRs")
    val rirUnicastIpv6 = Ports.toRecords(parseIpv6Records(ianaorg(IPV6_UNICAST_ASSIGNMENT))).fixIana

    val reallocatedSpecial = Ports.toRecords(ipv4Reallocated ++ ipv4SpecialRegistry).fixIana
    reallocatedSpecial.append(rirUnicastIpv6)
  }

  val RIRs = List("arin", "ripe", "apnic", "lacnic", "afrinic")
  val rangeRex = """(\d+)-(\d+)""".r
  val whoisRex = """whois.(\w+).net""".r
  val v4slash8 = """(\d\d\d)/8""".r
  val YYYYMM = """(\d\d\d\d)-(\d\d)""".r

  def parseAsnRecord(asnRecord: List[String]): List[String] = asnRecord match {
    case _ :: "Unallocated" :: _ => List()
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

    readCSV(asnNoHeader).map(parseAsnRecord).filter(_.nonEmpty)
  }

  def parseIpv4Records(ipv4Source: String) = {
    val ipv4Text = requests.get(ipv4Source).text()
    // Skipping header here.
    readCSV(ipv4Text).tail.map(buildIpv4Record)
  }

  def parseIpv6Records(ipv6Source: String) = {
    val ipv6Text = requests.get(ipv6Source).text()
    // Skipping header here.
    readCSV(ipv6Text).tail.map(buildIpv6Record).filter(_.nonEmpty)
  }

  def parseIpv6Special(ipv6Source: String) = {
    val ipv6Text = requests.get(ipv6Source).text()
    // Skipping header here.
    readCSV(ipv6Text).tail.map(buildIpv6Record).filter(_.nonEmpty)
  }

  def parseIpv4Recovered(ipv4Source: String) = {
    val ipv4Text = requests.get(ipv4Source).text()
    // Skipping header here.
    readCSV(ipv4Text).tail.map(buildIpv4Recovered)
  }

  def parseIpv4Reallocated(ipv4Source: String) = {
    val ipv4Text = requests.get(ipv4Source).text()
    // Skipping header here.
    readCSV(ipv4Text).tail.map(buildIpv4Reallocated)
  }

  def parseIpv4Special(ipv4Source: String) = {
    val ipv4Text = requests.get(ipv4Source).text()
    readCSV(ipv4Text).tail.map(buildIpv4SpecialRecord).filter(_.nonEmpty)
  }

  def buildIpv4Record(ipv4Record: List[String]): List[String] = ipv4Record match {
    case range :: _ :: date :: whois :: _ =>
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(range) ++ List(resolveDate(date), whoisRIR(whois))
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }

  def buildIpv6Record(ipv6Record: List[String]): List[String] = ipv6Record match {

    // Skip reserved
    case _ :: _ :: _ :: _ :: _ :: "RESERVED" :: _ => List()

    case range :: _ :: date :: whois :: _ =>
      List("iana", "ZZ", "ipv6") ++ toPrefixLength(range) ++ List(resolveDate(date), whoisRIR(whois))

    case _ => logger.error(s"Can't parse this line: $ipv6Record"); List()
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
    case _ => {
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
  }

  def buildIpv4Recovered(ipv4Record: List[String]): List[String] = ipv4Record match {
    case prefixStart :: prefixEnd :: _ :: date :: _ =>
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(prefixStart, prefixEnd) ++ List(resolveDate(date), s"iana")
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }

  def buildIpv4Reallocated(ipv4Record: List[String]): List[String] = ipv4Record match {
    case prefixStart :: prefixEnd :: rir :: date :: _ =>
      List("iana", "ZZ", "ipv4") ++ toPrefixLength(prefixStart, prefixEnd) ++ List(resolveDate(date), s"${rir.toLowerCase.replaceAll(" ", "")}")
    case _ => throw new Exception(s"Can't parse this line: $ipv4Record")
  }

  def buildIpv4SpecialRecord(ipv4Record: List[String]): List[String] = ipv4Record match {

    // Self conflicting and already marked as future use in ipv4-address-space, so skip.
    case "240.0.0.0/4" :: _ => List()
    case "255.255.255.255/32" :: _ => List()

    // Special registry which is claimed by ARIN and not included in current IANA.
    case "192.175.48.0/24" :: _ => List()

    // Can't parse this [reference], so special treatment.
    case "192.0.0.0/24 [2]" :: _ :: _ :: date :: _ => List("iana", "ZZ", "ipv4") ++ toPrefixLength("192.0.0.0/24") ++ List(resolveDate(date), "ietf")

    // Self conflicting small ranges are skipped, they are subset of others.
    case range :: _ :: _ :: date :: _  if (range.endsWith("/32") || range.endsWith("/29"))  => List()

    case range :: _ :: _ :: date :: _  =>  List("iana", "ZZ", "ipv4") ++ toPrefixLength(range) ++ List(resolveDate(date), "ietf")

    case _ => logger.error(s"Can't parse this line: $ipv4Record"); List()
  }

  def toPrefixLength(prefixStart: String, prefixEnd: String): List[String] = {
    val start = Ipv4.parse(prefixStart)
    val end = Ipv4.parse(prefixEnd)
    val range = Ipv4Range.from(start).to(end)
    List(s"$start", s"${range.size()}")
  }

  def readCSV(str: String): List[List[String]] = Using.resource(CSVReader.open(new StringReader(str)))(_.all).filter(_.nonEmpty)

}
