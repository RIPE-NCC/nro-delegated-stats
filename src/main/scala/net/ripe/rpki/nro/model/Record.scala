package net.ripe.rpki.nro.model

import java.math.BigInteger
import com.google.common.collect._
import net.ripe.commons.ip.{Ipv4, Ipv4Range, Ipv6Range, PrefixUtils}
import net.ripe.rpki.nro.main.Ranges

import scala.jdk.CollectionConverters._
import scala.collection.mutable

case class Stat(registry: String, cc: String, `type`: String, start: String, length: String, date: String, status: String, oid: String = "", ext: String = "e-stats") {
  def asList: List[String] = List(registry, cc, `type`, start, length, date, status, oid, ext)
  def noDate: List[String] = List(registry, cc, `type`, start, length, status, oid, ext)
}

case class Conflict(a: Record, b: Record) extends WithKey {
  override def toString: String = s"$a\n$b"
  def rirsInvolved = s"${a.registry}--${b.registry}"
  def key: List[String] = a.noDate ++ b.noDate
}

case class Unclaimed(record: Record) extends WithKey {
  override def key = record.noDate
  override def toString = record.toString
}

trait WithKey {
  def key: List[String]
}

trait Record extends Comparable[Record] with Ranges with WithKey {

  def stat: Stat
  def range: RecordRange

  def updateRange(key: Range[BigInteger]): Record
  def updateStat(stat: Stat): Record

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
  def key: List[String] = noDate
  override def toString: String = asList.mkString("|")

  override def compareTo(that: Record): Int = this.range.compareTo(that.range)

  def start: String = stat.start
  def length: String = stat.length
  def registry: String = stat.registry
  def `type`: String = stat.`type`
  def status: String = stat.status
  def ext: String = stat.ext
}

object Record {

  def apply(line: List[String]): Record = line match {

    case List(registry, cc, _type, start, length, date, status, oid, ext) => _type match {
      case "asn" => AsnRecord(Stat(registry, cc, "asn", start, length, date, status, oid, ext))
      case "ipv4" => Ipv4Record(Stat(registry, cc, "ipv4", start, length, date, status, oid, ext))
      case "ipv6" => Ipv6Record(Stat(registry, cc, "ipv6", start, length, date, status, oid, ext))
    }
    case List(registry, cc, _type, start, length, date, status, oid) => _type match {
      case "asn" => AsnRecord(Stat(registry, cc, "asn", start, length, date, status, oid))
      case "ipv4" => Ipv4Record(Stat(registry, cc, "ipv4", start, length, date, status, oid))
      case "ipv6" => Ipv6Record(Stat(registry, cc, "ipv6", start, length, date, status, oid))
    }
    case List(registry, cc, _type, start, length, date, status) => _type match {
      case "asn" => AsnRecord(Stat(registry, cc, "asn", start, length, date, status))
      case "ipv4" => Ipv4Record(Stat(registry, cc, "ipv4", start, length, date, status))
      case "ipv6" => Ipv6Record(Stat(registry, cc, "ipv6", start, length, date, status))
    }

    case _ => throw new IllegalArgumentException(s"Cannot parse record from line: $line")
  }
}

case class Ipv4Record(stat: Stat) extends Record {

  override def updateStat(stat: Stat): Record = Ipv4Record(stat)

  override def range: RecordRange = RecordRange.ipv4(start, length)

  override def merge(that: Record): Record = {
    val newLength = length.toLong + that.length.toLong
    this.copy(stat = this.stat.copy(length = s"$newLength"))
  }

  override def updateRange(key: Range[BigInteger]): Ipv4Record = {
    val start = toInterval(key)._1.longValue()
    val len = size(key)
    val startAddress = Ipv4.of(start)
    this.copy(stat = this.stat.copy(start = s"$startAddress", length = s"$len"))
  }

  def splitPrefixes(range: Range[BigInteger]): mutable.Seq[Ipv4Range] = {
    val (start, end) = toInterval(range)
    Ipv4Range.from(start).to(end).splitToPrefixes().asScala
  }
}

case class Ipv6Record(stat: Stat) extends Record {

  override def updateStat(info: Stat): Record = Ipv6Record(info)

  override def range: RecordRange = RecordRange.ipv6(start, length)

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

  override def updateRange(key: Range[BigInteger]): Record = {
    val (begin, end) = toInterval(key)
    val newRange: Ipv6Range = Ipv6Range.from(begin).to(end)
    val Array(start, prefix) = newRange.toStringInCidrNotation.split("/")
    this.copy(stat = this.stat.copy(start = start, length = prefix))
  }

  def splitPrefixes(range: Range[BigInteger]): mutable.Seq[Ipv6Range] = {
    val (start, end) = toInterval(range)
    Ipv6Range.from(start).to(end).splitToPrefixes().asScala
  }
}

case class AsnRecord(stat: Stat) extends Record {

  override def updateStat(info: Stat): Record = AsnRecord(info)

  override def range: RecordRange = RecordRange.asn(start, length)

  override def merge(that: Record): Record = {
    val newLength = length.toLong + that.length.toLong
    this.copy(stat = this.stat.copy(length = s"$newLength"))
  }

  override def updateRange(key: Range[BigInteger]): Record = {
    val start = toInterval(key)._1
    val length = size(key)
    this.copy(stat = this.stat.copy(start = s"$start", length = s"$length"))
  }
}


