package net.ripe.rpki.nro.model

import java.math.BigInteger

import com.google.common.collect.Range
import net.ripe.commons.ip.{Asn, AsnRange, Ipv4, Ipv4Range, Ipv6Range}

case class RecordRange(rStart: BigInteger, rEnd: BigInteger) extends Comparable[RecordRange] {

  def key: Range[BigInteger] = Range.closed(rStart, rEnd)

  def asn: AsnRange = AsnRange.from(rStart.longValue()).to(rEnd.longValue())

  def ipv4: Ipv4Range = Ipv4Range.from(rStart).to(rEnd)

  def ipv6: Ipv6Range = Ipv6Range.from(rStart).to(rEnd)

  def length: BigInteger = this.rEnd.subtract(this.rStart).add(BigInteger.ONE)

  def adjacent(that: RecordRange): Boolean = {
    this.rEnd.add(BigInteger.ONE) == that.rStart || that.rEnd.add(BigInteger.ONE) == this.rStart
  }

  override def compareTo(that: RecordRange): Int = {
    if (this.rStart == that.rStart) {
      this.rEnd.compareTo(that.rEnd)
    } else {
      this.rStart.compareTo(that.rStart)
    }
  }
}

object RecordRange {

  def ipv4(start: String, length: String): RecordRange = {
    val rStart = Ipv4.parse(start).asBigInteger()
    RecordRange(rStart, new BigInteger(length).add(rStart).subtract(BigInteger.ONE))
  }

  def ipv6(start: String, length: String): RecordRange = {
    val r = Ipv6Range.parseCidr(s"$start/$length")
    RecordRange(r.start().asBigInteger(), r.end().asBigInteger())
  }

  def asn(start: String, length: String): RecordRange = {
    val rStart: BigInteger = Asn.parse(start).asBigInteger()
    RecordRange(rStart, new BigInteger(length).add(rStart).subtract(BigInteger.ONE))
  }

  def from(asn: AsnRange)  : RecordRange = RecordRange(asn.start().asBigInteger(), asn.end().asBigInteger())

  def from(ipv4: Ipv4Range): RecordRange = RecordRange(ipv4.start().asBigInteger(), ipv4.end().asBigInteger())

  def from(ipv6: Ipv6Range): RecordRange = RecordRange(ipv6.start().asBigInteger(), ipv6.end().asBigInteger())

}