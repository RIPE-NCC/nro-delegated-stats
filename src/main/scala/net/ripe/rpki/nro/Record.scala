package net.ripe.rpki.nro

import java.math.BigInteger

import com.google.common.collect
import com.google.common.collect.BoundType
import net.ripe.ipresource._
import net.ripe.rpki.nro.Defs._

sealed trait Record {

  def update(key: collect.Range[BigInteger]): Record

  def registry: String
  def cc: String
  def lType: String
  def start: String
  def length: String
  def date: String
  def status: String
  def oid: String
  def ext: String

  def range: IpResource


  def merge(that: Record) : Record

  def canMerge(that: Record) : Boolean = {
        this.range.adjacent(that.range) &&
        this.cc == that.cc &&
        this.oid == that.oid &&
        this.date == that.date &&
        this.registry == that.registry &&
        this.status == that.status
  }


  def intersect(that: Record) : Boolean = {
      this.range.intersect(that.range) != null;
  }


  override def toString: String =
    List(registry, cc, lType, start, length, date, status, oid, ext).mkString("|")
}

object Record {
  def rangeLen(r: IpResource): BigInteger =
    r.getEnd.getValue.subtract(r.getStart.getValue).add(BigInteger.ONE)

  // help me name this thing.
  def startEndLen(r: collect.Range[BigInteger]) = {
    val start = if(r.lowerBoundType() == BoundType.CLOSED) r.lowerEndpoint() else r.lowerEndpoint().add(BigInteger.ONE)
    val end = if(r.upperBoundType() == BoundType.CLOSED) r.upperEndpoint() else r.upperEndpoint().subtract(BigInteger.ONE)
    val len = end.subtract(start).add(BigInteger.ONE)
    (start, end, len)
  }

  implicit val recordOrder = new Ordering[Record] {
    override def compare(a: Record, b: Record) = a.range.compareTo(b.range)
  }
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
  val range: IpResource = {
    val startAddr = IpAddress.parse(start).getValue
    val endAddr   = new BigInteger(length).add(startAddr).subtract(BigInteger.ONE)
    IpResourceRange.assemble(startAddr, endAddr, IpResourceType.IPv4)
  }

  override def merge(that: Record): Record = {
    val newLength = length.toLong + that.length.toLong
    this.copy(length = s"$newLength")
  }

  override def update(key: collect.Range[BigInteger]): Ipv4Record = {
    val (start, _, len) = Record.startEndLen(key)
    val startAddress  = new Ipv4Address(start.longValue());
    this.copy(start = s"$startAddress", length = s"$len")
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

  val range: IpResource = {
    IpResource.parse(start + "/" + length)
  }

  override def canMerge(that: Record): Boolean =  false

  override def merge(that: Record): Record = throw new Exception("Nope, don't do this")

  override def update(key: collect.Range[BigInteger]): Record = {
    val (begin, end, _) = Record.startEndLen(key)
    val newRange =  IpResourceRange.range(new Ipv6Address(begin), new Ipv6Address(end));
    val Array(start, prefix) = newRange.toString.split("/")
    this.copy(start = start, length = prefix)
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
  val range: IpResource = {
    val iLength = length.toLong
    val iStart  = start.toLong
    IpResource.parse(s"AS$iStart-AS${iStart + iLength - 1}")
  }

  override def merge(that: Record): Record = {
    val newLength = length.toLong + that.length.toLong
    this.copy(length = s"$newLength")
  }

  override def update(key: collect.Range[BigInteger]): Record = {
    val (start, _, length) = Record.startEndLen(key)
    this.copy(start = s"$start", length = s"$length")
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
