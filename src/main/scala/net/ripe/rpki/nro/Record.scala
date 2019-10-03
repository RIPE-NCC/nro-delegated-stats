package net.ripe.rpki.nro

import java.math.BigInteger

import com.google.common.collect._
import net.ripe.commons.ip.{Ipv4, Ipv6Range, PrefixUtils}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

case class Stat(registry: String, cc: String, lType: String, start: String, length: String, date: String, status: String, oid: String = "", ext: String = "e-stats") {
  def asList: List[String] = List(registry, cc, lType, start, length, date, status, oid, ext)
  def noDate: List[String] = List(registry, cc, lType, start, length, status, oid, ext)
}

case class Conflict(a: Record, b: Record) {

  override def toString: String = s"$a\n$b"

  def rirsInvolved = s"${a.registry}--${b.registry}"

  def key: List[String] = a.noDate ++ b.noDate
}

abstract class Record extends Comparable[Record] {

  def update(key: Range[BigInteger]): Record

  def stat: Stat

  def range: RecordRange

  def merge(that: Record): Record

  def canMerge(that: Record): Boolean = {
    this.range.adjacent(that.range) &&
      this.stat.cc == that.stat.cc &&
      this.stat.oid == that.stat.oid &&
      this.stat.date == that.stat.date &&
      this.stat.registry == that.stat.registry &&
      this.stat.status == that.stat.status
  }

  def asList: List[String] = stat.asList

  def noDate: List[String] = stat.noDate

  override def toString: String = asList.mkString("|")

  override def compareTo(that: Record): Int = this.range.compareTo(that.range)

  def update(info: Stat): Record

  def start: String = stat.start

  def length: String = stat.length

  def registry: String = stat.registry

  def lType: String = stat.lType

  def status: String = stat.status

  def ext: String = stat.ext
}

object Record {

  def apply(line: List[String]): Record = line match {

    case List(registry, cc, ltype, start, length, date, status, oid, ext) => ltype match {
      case "asn" => AsnRecord(Stat(registry, cc, "asn", start, length, date, status, oid, ext))
      case "ipv4" => Ipv4Record(Stat(registry, cc, "ipv4", start, length, date, status, oid, ext))
      case "ipv6" => Ipv6Record(Stat(registry, cc, "ipv6", start, length, date, status, oid, ext))
    }
    case List(registry, cc, ltype, start, length, date, status, oid) => ltype match {
      case "asn" => AsnRecord(Stat(registry, cc, "asn", start, length, date, status, oid))
      case "ipv4" => Ipv4Record(Stat(registry, cc, "ipv4", start, length, date, status, oid))
      case "ipv6" => Ipv6Record(Stat(registry, cc, "ipv6", start, length, date, status, oid))
    }
    case List(registry, cc, ltype, start, length, date, status) => ltype match {
      case "asn" => AsnRecord(Stat(registry, cc, "asn", start, length, date, status))
      case "ipv4" => Ipv4Record(Stat(registry, cc, "ipv4", start, length, date, status))
      case "ipv6" => Ipv6Record(Stat(registry, cc, "ipv6", start, length, date, status))
    }
  }
}

case class Ipv4Record(stat: Stat) extends Record {

  override def update(stat: Stat): Record = Ipv4Record(stat)

  val range: RecordRange = RecordRange.ipv4(start, length)

  override def merge(that: Record): Record = {
    val newLength = length.toLong + that.length.toLong
    this.copy(stat = this.stat.copy(length = s"$newLength"))
  }

  override def update(key: Range[BigInteger]): Ipv4Record = {
    val start = Ranges.toInterval(key)._1.longValue()
    val len = Ranges.length(key)
    val startAddress = Ipv4.of(start)
    this.copy(stat = this.stat.copy(start = s"$startAddress", length = s"$len"))
  }
}

case class Ipv6Record(stat: Stat) extends Record {

  override def update(info: Stat): Record = Ipv6Record(info)

  val range: RecordRange = RecordRange.ipv6(start, length)

  private def legalMerge(a: RecordRange, b: RecordRange): Boolean = {
    PrefixUtils.isLegalPrefix(a.ipv6.merge(b.ipv6))
  }

  override def canMerge(that: Record): Boolean = {
    super.canMerge(that) && legalMerge(this.range, that.range)
  }

  override def merge(that: Record): Record = {
    val Array(start, length) = range.ipv6.merge(that.range.ipv6).toStringInCidrNotation.split("/")
    this.copy(stat = this.stat.copy(start = s"$start", length = s"$length"))
  }

  override def update(key: Range[BigInteger]): Record = {
    val (begin, end) = Ranges.toInterval(key)
    val newRange: Ipv6Range = Ipv6Range.from(begin).to(end)
    val Array(start, prefix) = newRange.toStringInCidrNotation.split("/")
    this.copy(stat = this.stat.copy(start = start, length = prefix))
  }
}

case class AsnRecord(stat: Stat) extends Record {

  override def update(info: Stat): Record = AsnRecord(info)

  val range: RecordRange = RecordRange.asn(start, length)

  override def merge(that: Record): Record = {
    val newLength = length.toLong + that.length.toLong
    this.copy(stat = this.stat.copy(length = s"$newLength"))
  }

  override def update(key: Range[BigInteger]): Record = {
    val start = Ranges.toInterval(key)._1
    val length = Ranges.length(key)
    this.copy(stat = this.stat.copy(start = s"$start", length = s"$length"))
  }
}

object Ipv6Record {
  def splitPrefixes(range: Range[BigInteger]): mutable.Seq[Ipv6Range] = {
    val (start, end) = Ranges.toInterval(range)
    Ipv6Range.from(start).to(end).splitToPrefixes().asScala
  }
}

