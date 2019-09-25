package net.ripe.rpki.nro

import java.math.BigInteger

import com.google.common.collect
import com.google.common.collect.{BoundType, DiscreteDomain, Range}
import net.ripe.commons.ip.{Ipv6Range, PrefixUtils}
import net.ripe.ipresource._
import net.ripe.rpki.nro.Defs._

import scala.jdk.CollectionConverters._
import scala.collection.mutable

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

  def key: collect.Range[BigInteger] =
    Range.closed(range.getStart.getValue, range.getEnd.getValue)

  private val discreteBigInteger = DiscreteDomain.bigIntegers()

  def empty: Boolean = key.canonical(discreteBigInteger).isEmpty

  def merge(that: Record): Record

  def canMerge(that: Record): Boolean = {
    this.range.adjacent(that.range) &&
      this.cc == that.cc &&
      this.oid == that.oid &&
      this.date == that.date &&
      this.registry == that.registry &&
      this.status == that.status
  }

  def asList = List(registry, cc, lType, start, length, date, status, oid, ext)

  def noDate = List(registry, cc, lType, start, length, status, oid, ext)

  override def toString: String = {
    asList.mkString("|")
  }
}

object Record {

  def apply(line: List[String]): Record = line match {

    case List(registry, cc, ltype, start, length, date, status, oid, ext) => ltype match {
      case "asn" => AsnRecord(registry, cc, "asn", start, length, date, status, oid, ext)
      case "ipv4" => Ipv4Record(registry, cc, "ipv4", start, length, date, status, oid, ext)
      case "ipv6" => Ipv6Record(registry, cc, "ipv6", start, length, date, status, oid, ext)
    }

    case List(registry, cc, ltype, start, length, date, status, oid) => ltype match {
      case "asn" => AsnRecord(registry, cc, "asn", start, length, date, status, oid)
      case "ipv4" => Ipv4Record(registry, cc, "ipv4", start, length, date, status, oid)
      case "ipv6" => Ipv6Record(registry, cc, "ipv6", start, length, date, status, oid
      )
    }

    case List(registry, cc, ltype, start, length, date, status) => ltype match {
      case "asn" => AsnRecord(registry, cc, "asn", start, length, date, status)
      case "ipv4" => Ipv4Record(registry, cc, "ipv4", start, length, date, status)
      case "ipv6" => Ipv6Record(registry, cc, "ipv6", start, length, date, status)
    }

  }

  def length(r: IpResource): BigInteger =
    r.getEnd.getValue.subtract(r.getStart.getValue).add(BigInteger.ONE)

  def toInterval(r: collect.Range[BigInteger]): (BigInteger, BigInteger) = {
    val start = if (r.lowerBoundType() == BoundType.CLOSED) {
      r.lowerEndpoint()
    } else {
      r.lowerEndpoint().add(BigInteger.ONE)
    }
    val end = if (r.upperBoundType() == BoundType.CLOSED) {
      r.upperEndpoint()
    } else {
      r.upperEndpoint().subtract(BigInteger.ONE)
    }
    (start, end)
  }

  def length(r: collect.Range[BigInteger]): BigInteger = {
    val (start, end) = toInterval(r)
    end.subtract(start).add(BigInteger.ONE)
  }

  implicit val recordOrder: Ordering[Record] = (a: Record, b: Record) => a.range.compareTo(b.range)
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
                       ext: String = DEFAULT_EXT
                     ) extends Record {
  val range: IpResource = {
    val startAddr = IpAddress.parse(start).getValue
    val endAddr = new BigInteger(length).add(startAddr).subtract(BigInteger.ONE)
    IpResourceRange.assemble(startAddr, endAddr, IpResourceType.IPv4)
  }

  override def merge(that: Record): Record = {
    val newLength = length.toLong + that.length.toLong
    this.copy(length = s"$newLength")
  }

  override def update(key: collect.Range[BigInteger]): Ipv4Record = {
    val start = Record.toInterval(key)._1.longValue()
    val len = Record.length(key)
    val startAddress = new Ipv4Address(start)
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
                       oid: String = "",
                       ext: String = DEFAULT_EXT
                     ) extends Record {

  val range: IpResource = {
    IpResource.parse(start + "/" + length)
  }

  private def legalMerge(a: IpResource, b: IpResource): Boolean = {
    PrefixUtils.isLegalPrefix(Ipv6Range.parse(a.merge(b).toString))
  }

  override def canMerge(that: Record): Boolean = {
    super.canMerge(that) && legalMerge(this.range, that.range)
  }

  override def merge(that: Record): Record = {
    val merged = this.range.merge(that.range)
    val Array(start, length) = merged.toString.split("/")
    this.copy(start = s"$start", length = s"$length")
  }

  override def update(key: collect.Range[BigInteger]): Record = {
    val (begin, end) = Record.toInterval(key)
    val newRange: Ipv6Range = Ipv6Range.from(begin).to(end)
    val Array(start, prefix) = newRange.toStringInCidrNotation.split("/")
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
                      oid: String = "",
                      ext: String = DEFAULT_EXT
                    ) extends Record {
  val range: IpResource = {
    val iLength = length.toLong
    val iStart = start.toLong
    IpResource.parse(s"AS$iStart-AS${iStart + iLength - 1}")
  }

  override def merge(that: Record): Record = {
    val newLength = length.toLong + that.length.toLong
    this.copy(length = s"$newLength")
  }

  override def update(key: collect.Range[BigInteger]): Record = {
    val start = Record.toInterval(key)._1
    val length = Record.length(key)
    this.copy(start = s"$start", length = s"$length")
  }
}


object Ipv6Record {
  def splitPrefixes(range: Range[BigInteger]): mutable.Buffer[Ipv6Range] = {
    val (start, end) = Record.toInterval(range)
    Ipv6Range.from(start).to(end).splitToPrefixes().asScala
  }
}

case class Conflict(a: Record, b: Record) {

  override def toString: String = s"$a\n$b"
  def rirsInvolved = s"${a.registry}--${b.registry}"

  def key: List[String] = a.noDate ++ b.noDate
}

