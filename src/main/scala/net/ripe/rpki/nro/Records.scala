package net.ripe.rpki.nro

import net.ripe.rpki.nro.Const._
import net.ripe.rpki.nro.Ranges._
import net.ripe.rpki.nro.Records._
import net.ripe.rpki.nro.Settings._

import scala.util.chaining._

case class Records(asn: List[Record], ipv4: List[Record], ipv6: List[Record]) {

  def fixUnavailable: Records = {
    def fix[R]: Record => Record = (rec: Record) => {
      val registry = rec.status
      rec.pipe(date_(TODAY)).pipe(status_(AVAILABLE)).pipe(registry_(registry))
    }
    fixRecords(fix)
  }

  def fixIana: Records = {
    def fix[R]: Record => Record = (rec: Record) => rec.status match {
      case IETF => rec.pipe(oid_(IETF)).pipe(ext_(IANA))
      case IANA => rec.pipe(oid_(IANA)).pipe(status_(ASSIGNED)).pipe(ext_(IANA))
      case _    => rec.pipe(ext_(IANA))
    }
    fixRecords(fix)
  }

  def fixRIRs: Records = {
    def fix: Record => Record = (rec: Record) => rec.status match {
      // RESERVED and AVAILABLE has neither date nor country (except AFRINIC with ZZ)
      case RESERVED | AVAILABLE => rec.pipe(date_(TODAY)).pipe(cc_(DEFAULT_CC))
      // Whatever allocated in original RIR file appears as ASSIGNED
      case ALLOCATED => rec.pipe(status_(ASSIGNED))
      case _ => rec
    }
    fixRecords(fix)
  }

  def fixRecords(fix: Record => Record): Records = Records(
    this.asn.map(fix),
    this.ipv4.map(fix),
    this.ipv6.map(fix))

  def substract(that: Records): Records = {
    val (thisAsn, thisIpv4, thisIpv6) = (asRangeMap(this.asn), asRangeMap(this.ipv4), asRangeMap(this.ipv6))
    val (thatAsn, thatIpv4, thatIpv6) = (asRangeMap(that.asn), asRangeMap(that.ipv4), asRangeMap(that.ipv6))

    thatAsn.asMapOfRanges() .forEach { case (key, _) => thisAsn.remove(key) }
    thatIpv4.asMapOfRanges().forEach { case (key, _) => thisIpv4.remove(key) }
    thatIpv6.asMapOfRanges().forEach { case (key, _) => thisIpv6.remove(key) }

    Records(updateMap(thisAsn), updateMap(thisIpv4), updateMap(thisIpv6))
  }

  def append(that: Records): Records =
    Records(this.asn ++ that.asn, this.ipv4 ++ that.ipv4, this.ipv6 ++ that.ipv6)

  def sorted() : Records =
    Records(this.asn.sorted, this.ipv4.sorted, this.ipv6.sorted)

  def partition(criteria: Record => Boolean): (Records, Records) = {
    val (asnRirs,  asnNonRirs)  = this.asn.partition (criteria)
    val (ipv4Rirs, ipv4NonRirs) = this.ipv4.partition(criteria)
    val (ipv6Rirs, ipv6NonRirs) = this.ipv6.partition(criteria)

    (Records(asnRirs, ipv4Rirs, ipv6Rirs), Records(asnNonRirs, ipv4NonRirs, ipv6NonRirs) )
  }
}

object Records {

  // Exposing copy so that we can do generic update operations on Record (Asn, Ipv4, Ipv6)
  def oid_(oid: String): Record => Record = (r: Record) => r match {
    case a: AsnRecord => a.copy(oid = oid)
    case a: Ipv4Record => a.copy(oid = oid)
    case a: Ipv6Record => a.copy(oid = oid)
  }

  def ext_(ext: String): Record => Record = (r: Record) => r match {
    case a: AsnRecord => a.copy(ext = ext)
    case a: Ipv4Record => a.copy(ext = ext)
    case a: Ipv6Record => a.copy(ext = ext)
  }

  def date_(date: String): Record => Record = (r: Record) => r match {
    case a: AsnRecord => a.copy(date = date)
    case a: Ipv4Record => a.copy(date = date)
    case a: Ipv6Record => a.copy(date = date)
  }

  def status_(status: String): Record => Record = (r: Record) => r match {
    case a: AsnRecord => a.copy(status = status)
    case a: Ipv4Record => a.copy(status = status)
    case a: Ipv6Record => a.copy(status = status)
  }

  def cc_(cc: String): Record => Record = (r: Record) => r match {
    case a: AsnRecord => a.copy(cc = cc)
    case a: Ipv4Record => a.copy(cc = cc)
    case a: Ipv6Record => a.copy(cc = cc)
  }

  def registry_(registry: String): Record => Record = (r: Record) => r match {
    case a: AsnRecord => a.copy (registry = registry)
    case a: Ipv4Record => a.copy(registry = registry)
    case a: Ipv6Record => a.copy(registry = registry)
  }
}
