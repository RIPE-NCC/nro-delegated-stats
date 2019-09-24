package net.ripe.rpki.nro

import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Updates._
import net.ripe.rpki.nro.Configs._

case class Records(asn: List[AsnRecord], ipv4: List[Ipv4Record], ipv6: List[Ipv6Record]) {

  def fixIana: Records = {
    def fix[A <: Record : Updates]: A => A = (rec: A) => rec.status match {
      case IETF => rec.oid_(IETF).ext_(IANA)
      case IANA => rec.oid_(IANA).status_(ASSIGNED).ext_(IANA)
      case _ => rec.ext_(IANA)
    }
    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }

  // Reserved and available records normally have neither country code nor date,
  // when combined it will be filled with ZZ and today's date.
  def fixRIRs: Records = {
    def fix[A <: Record : Updates]: A => A = (rec: A) => rec.status match {
      // RESERVED and AVAILABLE has neither date nor country (except AFRINIC with ZZ)
      case RESERVED | AVAILABLE => rec.date_(TODAY).cc_(DEFAULT_CC)
      // Whatever allocated in original RIR file appears as ASSIGNED
      case ALLOCATED => rec.status_(ASSIGNED)
      case _ => rec
    }
    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }


  // Map values wrapper
  def asn_(f: AsnRecord => AsnRecord)(implicit ev: Updates[AsnRecord]): Records =
    this.copy(asn = this.asn.map(f))

  def ipv4_(f: Ipv4Record => Ipv4Record)(implicit ev: Updates[Ipv4Record]): Records =
    this.copy(ipv4 = this.ipv4.map(f))

  def ipv6_(f: Ipv6Record => Ipv6Record)(implicit ev: Updates[Ipv6Record]): Records =
    this.copy(ipv6 = this.ipv6.map(f))

}

