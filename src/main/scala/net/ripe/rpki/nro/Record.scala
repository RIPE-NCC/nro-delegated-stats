package net.ripe.rpki.nro

import java.math.BigInteger

import net.ripe.ipresource.{IpAddress, IpResource, IpResourceRange, IpResourceType}
import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Record._

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

object Record {
  def rangeLen(r: IpResource): BigInteger =
    r.getEnd.getValue.subtract(r.getStart.getValue).add(BigInteger.ONE)
}

case class Ipv4Record(
    registry: String,
    cc: String,
    lType: String,
    start: String,
    length: String,
    date: String,
    status: String,
    oid: String,
    ext: String = DEFAULT_EXT
) extends Record {
  def range(): IpResourceRange = {
    val startAddr = IpAddress.parse(start).getValue
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
    oid: String,
    ext: String = DEFAULT_EXT
) extends Record {
  def range(): IpResourceRange = {
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
    oid: String,
    ext: String = DEFAULT_EXT
) extends Record {
  def range(): IpResourceRange = {
    val iLength = length.toLong
    val iStart  = start.toLong
    IpResourceRange.parse(s"AS$iStart-AS${iStart + iLength - 1}")
  }
}

object Ipv4Record {
  def apply(rec: Array[String]): Ipv4Record = {
    val oid = if (rec.length > 7) rec(7) else ""
    new Ipv4Record(rec(0), rec(1), rec(2), rec(3), rec(4), rec(5), rec(6), oid)
  }

  // Ipv4 ianapool on Jeff's combined results always dated 20120801, Magic date, where is it from? The ASN and Ipv6 is dated TODAY
  def ianapool(ipv4: IpResource) =
    Ipv4Record( IANA, DEFAULT_CC, IPV4, ipv4.getStart + "", rangeLen(ipv4) + "", IPV4_IANA_POOL_DATE, IANAPOOL, "", IANA)

}

object Ipv6Record {
  def apply(rec: Array[String]): Ipv6Record = {

    val oid = if (rec.length > 7) rec(7) else ""
    // Normalize data from iana input 4000:0000 into 4000::
    val ipv6                 = IpResourceRange.parse(rec(3) + "/" + rec(4))
    val Array(start, prefix) = ipv6.toString.split("/")
    new Ipv6Record(rec(0), rec(1), rec(2), start, prefix, rec(5), rec(6), oid)
  }

  def ianapool(ipv6: IpResource): Ipv6Record = {
    val Array(start, prefix) = ipv6.toString.split("/")
    Ipv6Record(IANA, DEFAULT_CC, IPV6, start, prefix, TODAY, IANAPOOL, "", IANA)
  }

}

object AsnRecord {
  def apply(rec: Array[String]): AsnRecord = {
    val oid = if (rec.length > 7) rec(7) else ""
    new AsnRecord(rec(0), rec(1), rec(2), rec(3), rec(4), rec(5), rec(6), oid)
  }

  def ianapool(asn: IpResource) =
    AsnRecord( IANA, DEFAULT_CC, ASN, asn.getStart.getValue + "", rangeLen(asn) + "", TODAY, IANAPOOL, "", IANA)

}
