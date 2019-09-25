package net.ripe.rpki.nro

import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Settings._
import scala.util.chaining._
import Records._

case class Records(asn: List[AsnRecord], ipv4: List[Ipv4Record], ipv6: List[Ipv6Record]) {

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

  def fixRecords(fix: Record => Record): Records = {
    this.copy(
      asn  = this.asn.map(fix) .map(_.asInstanceOf[AsnRecord]),
      ipv4 = this.ipv4.map(fix).map(_.asInstanceOf[Ipv4Record]),
      ipv6 = this.ipv6.map(fix).map(_.asInstanceOf[Ipv6Record])
    )
  }

}

// Exposing copy so that we can do generic update operations on Record (Asn, Ipv4, Ipv6)
object Records {

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
}
