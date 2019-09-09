package net.ripe.rpki.nro

import java.math.BigInteger
import net.ripe.ipresource.{IpAddress, IpResourceRange, IpResourceType}
import scala.collection.SortedMap

import Defs._

sealed trait Record {
  def range(): IpResourceRange
  def registry: String
  def cc: String
  def lType: String
  def start: String
  def length: String
  def date: String
  def status: String
  def oid: String
  def ext: String

  override def toString: String =
    List(registry, cc, lType, start, length, date, status, oid, ext).mkString("|")
}

case class Ipv4Record(
    registry: String,
    cc: String,
    lType: String,
    start: String,
    length: String,
    date: String,
    status: String,
    oid: String = "",
    ext: String = ""
) extends Record {
  def range() = {
    val startAddr = IpAddress.parse(start).getValue()
    val endAddr   = new BigInteger(length).add(startAddr).subtract(BigInteger.ONE)
    IpResourceRange.assemble(startAddr, endAddr, IpResourceType.IPv4)
  }
}

case class Ipv6Record(
    registry: String,
    cc: String,
    lType: String,
    start: String,
    length: String,
    date: String,
    status: String,
    oid: String = "",
    ext: String = ""
) extends Record {
  def range() = {
    IpResourceRange.parse(start + "/" + length)
  }
}

case class AsnRecord(
    registry: String,
    cc: String,
    lType: String,
    start: String,
    length: String,
    date: String,
    status: String,
    oid: String = "",
    ext: String = ""
) extends Record {
  def range() = {
    val iLength = length.toLong
    val iStart  = start.toLong
    IpResourceRange.parse(s"AS$iStart-AS${iStart + iLength - 1}")
  }
}

object Ipv4Record {
  def apply(rec: Array[String]) = {
    val oid = if (rec.length > 7) rec(7) else ""
    val ext = if (rec.length > 8) rec(8) else ""
    new Ipv4Record(rec(0), rec(1), rec(2), rec(3), rec(4), rec(5), rec(6), oid, ext)
  }

  // Ipv4 ianapool on Jeff's combined results always dated 20120801, Magic date, where is it from? The ASN and Ipv6 is dated TODAY
  def ianapool(ipv4: IpResourceRange) =
    Ipv4Record(
      "iana",
      "ZZ",
      "ipv4",
      ipv4.getStart + "",
      rangeLen(ipv4) + "",
      "20120801",
      "ianapool",
      "",
      "iana"
    )

  implicit val updates = new Updates[Ipv4Record] {
    def status_(r: Ipv4Record, status: String) = r.copy(status = status)
    def ext_(r: Ipv4Record, ext: String)       = r.copy(ext = ext)
    def oid_(r: Ipv4Record, oid: String)       = r.copy(oid = oid)
    def cc_(r: Ipv4Record, cc: String)         = r.copy(cc = cc)
    def date_(r: Ipv4Record, date: String)     = r.copy(date = date)
  }
}

object Ipv6Record {
  def apply(rec: Array[String]) = {
    val oid = if (rec.length > 7) rec(7) else ""
    val ext = if (rec.length > 8) rec(8) else ""

    // Normalize data from iana 4000:0000 instead of 4000::
    val ipv6                 = IpResourceRange.parse(rec(3) + "/" + rec(4))
    val Array(start, prefix) = ipv6.toString.split("/")
    new Ipv6Record(rec(0), rec(1), rec(2), start, prefix, rec(5), rec(6), oid, ext)
  }

  def ianapool(ipv6: IpResourceRange) = {
    val Array(start, prefix) = ipv6.toString.split("/")
    Ipv6Record("iana", "ZZ", "ipv6", start, prefix, TODAY, "ianapool", "", "iana")
  }

  implicit val updates = new Updates[Ipv6Record] {
    def status_(r: Ipv6Record, status: String) = r.copy(status = status)
    def ext_(r: Ipv6Record, ext: String)       = r.copy(ext = ext)
    def oid_(r: Ipv6Record, oid: String)       = r.copy(oid = oid)
    def cc_(r: Ipv6Record, cc: String)         = r.copy(cc = cc)
    def date_(r: Ipv6Record, date: String)     = r.copy(date = date)
  }

}

object AsnRecord {
  def apply(rec: Array[String]) = {
    val oid = if (rec.length > 7) rec(7) else ""
    val ext = if (rec.length > 8) rec(8) else ""
    new AsnRecord(rec(0), rec(1), rec(2), rec(3), rec(4), rec(5), rec(6), oid, ext)
  }

  def ianapool(asn: IpResourceRange) =
    AsnRecord(
      "iana",
      "ZZ",
      "asn",
      asn.getStart.getValue + "",
      rangeLen(asn) + "",
      TODAY,
      "ianapool",
      "",
      "iana"
    )

  implicit val updates = new Updates[AsnRecord] {
    def status_(r: AsnRecord, status: String) = r.copy(status = status)
    def ext_(r: AsnRecord, ext: String)       = r.copy(ext = ext)
    def oid_(r: AsnRecord, oid: String)       = r.copy(oid = oid)
    def cc_(r: AsnRecord, oid: String)        = r.copy(cc = oid)
    def date_(r: AsnRecord, oid: String)      = r.copy(date = oid)
  }

}

case class Summary(registry: String, lType: String, count: String)
case class Header(
    version: String,
    registry: String,
    serial: String,
    records: String,
    startDate: String,
    endDate: String,
    utcOffset: String
)

case class Records(
    source: String,
    header: Line,
    summaries: List[Line],
    asn: SortedMap[IpResourceRange, AsnRecord],
    ipv4: SortedMap[IpResourceRange, Ipv4Record],
    ipv6: SortedMap[IpResourceRange, Ipv6Record]
) {
  override def toString: String =
    s"""Source: $source \nHeader: ${header.mkString(",")} \nSummaries: \n${summaries
      .map(_.mkString(","))
      .mkString("\n")}\n\n"""

  // ext field will be adjusted according to the source. (what about oid?)
  def fixExt: Records = {

    val ext                                  = if (source == "iana") "iana" else "e-stats"
    def fixExt[A <: Record: Updates]: A => A = _.ext_(ext)

    val fAsn  = this.asn.mapValues(fixExt)
    val fIpv4 = this.ipv4.mapValues(fixExt)
    val fIpv6 = this.ipv6.mapValues(fixExt)

    this.copy(asn = fAsn, ipv4 = fIpv4, ipv6 = fIpv6)
  }

  def fixIetfIana: Records = {

    def fixIetfIana[A <: Record: Updates]: A => A = (a: A) => {
      if (a.status == "ietf") a.oid_("ietf").ext_("iana")
      else if (a.status == "iana") a.oid_("iana").status_("assigned").ext_("iana")
      else a
    }

    val fAsn  = this.asn.mapValues(fixIetfIana)
    val fIpv4 = this.ipv4.mapValues(fixIetfIana)
    val fIpv6 = this.ipv6.mapValues(fixIetfIana)

    this.copy(asn = fAsn, ipv4 = fIpv4, ipv6 = fIpv6)
  }

  // Reserved and available records normally have neither country code or date, when combined it will be filled with ZZ and today's date.
  def fixReservedAvailable: Records = {
    val cc = "ZZ"
    def fixReservedAvailable[A <: Record: Updates]: A => A =
      (a: A) => if (a.status == "reserved" || a.status == "available") a.date_(TODAY).cc_(cc) else a

    val fAsn  = this.asn.mapValues(fixReservedAvailable)
    val fIpv4 = this.ipv4.mapValues(fixReservedAvailable)
    val fIpv6 = this.ipv6.mapValues(fixReservedAvailable)

    this.copy(asn = fAsn, ipv4 = fIpv4, ipv6 = fIpv6)
  }

  // RIR's allocated are all converted to assigned when combined.
  def fixAllocated: Records = {

    def fixAllocated[A <: Record: Updates]: A => A =
      (a: A) => if (a.status == "allocated") a.status_("assigned") else a

    val fAsn  = this.asn.mapValues(fixAllocated)
    val fIpv4 = this.ipv4.mapValues(fixAllocated)
    val fIpv6 = this.ipv6.mapValues(fixAllocated)

    this.copy(asn = fAsn, ipv4 = fIpv4, ipv6 = fIpv6)
  }

}
