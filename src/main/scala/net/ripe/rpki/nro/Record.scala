package net.ripe.rpki.nro

import java.math.BigInteger

import com.google.common.collect._
import net.ripe.commons.ip.{Ipv4, Ipv6Range, PrefixUtils}
import net.ripe.rpki.nro.Const._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

sealed trait Record extends Comparable[Record]{

  def update(key: Range[BigInteger]): Record

  def registry: String

  def cc: String

  def lType: String

  def start: String

  def length: String

  def date: String

  def status: String

  def oid: String

  def ext: String

  def range: RecordRange

  def merge(that: Record): Record

  def canMerge(that: Record): Boolean = {
    this.range.adjacent(that.range) &&
      this.cc == that.cc &&
      this.oid == that.oid &&
      this.date == that.date &&
      this.registry == that.registry &&
      this.status == that.status
  }

  def asList: List[String] = List(registry, cc, lType, start, length, date, status, oid, ext)

  def noDate: List[String] = List(registry, cc, lType, start, length, status, oid, ext)

  override def toString: String = asList.mkString("|")

  override def compareTo(that: Record): Int = this.range.compareTo(that.range)
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

  val range: RecordRange = RecordRange.ipv4(start, length)

  override def merge(that: Record): Record = {
    val newLength = length.toLong + that.length.toLong
    this.copy(length = s"$newLength")
  }

  override def update(key: Range[BigInteger]): Ipv4Record = {
    val start = Ranges.toInterval(key)._1.longValue()
    val len = Ranges.length(key)
    val startAddress = Ipv4.of(start)
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

  val range: RecordRange = RecordRange.ipv6(start,length)

  private def legalMerge(a: RecordRange, b: RecordRange): Boolean = {
    PrefixUtils.isLegalPrefix(a.ipv6.merge(b.ipv6))
  }

  override def canMerge(that: Record): Boolean = {
    super.canMerge(that) && legalMerge(this.range, that.range)
  }

  override def merge(that: Record): Record = {
    val Array(start, length) = range.ipv6.merge(that.range.ipv6).toStringInCidrNotation.split("/")
    this.copy(start = s"$start", length = s"$length")
  }

  override def update(key: Range[BigInteger]): Record = {
    val (begin, end) = Ranges.toInterval(key)
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

  val range: RecordRange = RecordRange.asn(start, length)

  override def merge(that: Record): Record = {
    val newLength = length.toLong + that.length.toLong
    this.copy(length = s"$newLength")
  }

  override def update(key: Range[BigInteger]): Record = {
    val start = Ranges.toInterval(key)._1
    val length = Ranges.length(key)
    this.copy(start = s"$start", length = s"$length")
  }
}

object Ipv6Record {
  def splitPrefixes(range: Range[BigInteger]): mutable.Seq[Ipv6Range] = {
    val (start, end) = Ranges.toInterval(range)
    Ipv6Range.from(start).to(end).splitToPrefixes().asScala
  }
}

case class Conflict(a: Record, b: Record) {

  override def toString: String = s"$a\n$b"

  def rirsInvolved = s"${a.registry}--${b.registry}"

  def key: List[String] = a.noDate ++ b.noDate
}

