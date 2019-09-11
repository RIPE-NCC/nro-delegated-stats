package net.ripe.rpki.nro

import java.math.BigInteger

import net.ripe.ipresource._
import net.ripe.rpki.nro.Defs._

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

  def range(): IpResource

  override def toString: String =
    List(registry, cc, lType, start, length, date, status, oid, ext).mkString("|")
}

object Record {
  def rangeLen(r: IpResource): BigInteger =
    r.getEnd.getValue.subtract(r.getStart.getValue).add(BigInteger.ONE)
}

case class Conflict(a : Record, b: Record, kind: String = "inter-rir"){
  override def toString: String = s"\n$kind:\n<$a\n>$b"
  def rirsInvolved = s"${a.registry}--${b.registry}"
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
  def range(): IpResource = {
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
  def range(): IpResource = {
    IpResource.parse(start + "/" + length)
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
  def range(): IpResource = {
    val iLength = length.toLong
    val iStart  = start.toLong
    IpResource.parse(s"AS$iStart-AS${iStart + iLength - 1}")
  }
}

object Ipv4Record {
  def apply(rec: Array[String]): Ipv4Record = {
    val oid = if (rec.length > 7) rec(7) else ""
    new Ipv4Record(rec(0), rec(1), rec(2), rec(3), rec(4), rec(5), rec(6), oid)
  }
}

object Ipv6Record {
  def apply(rec: Array[String]): Ipv6Record = {

    val oid = if (rec.length > 7) rec(7) else ""
    // Normalize data from iana input 4000:0000 into 4000::
    val ipv6                 = IpResourceRange.parse(rec(3) + "/" + rec(4))
    val Array(start, prefix) = ipv6.toString.split("/")
    new Ipv6Record(rec(0), rec(1), rec(2), start, prefix, rec(5), rec(6), oid)
  }
}

object AsnRecord {
  def apply(rec: Array[String]): AsnRecord = {
    val oid = if (rec.length > 7) rec(7) else ""
    new AsnRecord(rec(0), rec(1), rec(2), rec(3), rec(4), rec(5), rec(6), oid)
  }
}
