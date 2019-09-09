package net.ripe.rpki.nro

import java.math.BigInteger
import net.ripe.ipresource.{IpAddress, IpResourceRange, IpResourceType}
import scala.collection.SortedMap

import Defs._
import Updates._

sealed trait Record {
  def registry: String
  def cc: String
  def lType: String
  def start: String
  def length: String
  def date: String
  def status: String
  def oid: String
  def ext: String

  def range(): IpResourceRange
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
    Ipv4Record( "iana", "ZZ", "ipv4", ipv4.getStart + "", rangeLen(ipv4) + "", "20120801", "ianapool", "", "iana")

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

}

object AsnRecord {
  def apply(rec: Array[String]) = {
    val oid = if (rec.length > 7) rec(7) else ""
    val ext = if (rec.length > 8) rec(8) else ""
    new AsnRecord(rec(0), rec(1), rec(2), rec(3), rec(4), rec(5), rec(6), oid, ext)
  }

  def ianapool(asn: IpResourceRange) =
    AsnRecord( "iana", "ZZ", "asn", asn.getStart.getValue + "", rangeLen(asn) + "", TODAY, "ianapool", "", "iana")

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

