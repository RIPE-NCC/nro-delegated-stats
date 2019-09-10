package net.ripe.rpki.nro 

import net.ripe.ipresource.IpResourceRange
import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Updates._

import scala.collection.SortedMap

// Records holder for three type of records, contains some logic of fixing entries
case class Records(
    source: String,
    header: Line,
    summaries: List[Line],
    asn:  SortedMap[IpResourceRange, AsnRecord],
    ipv4: SortedMap[IpResourceRange, Ipv4Record],
    ipv6: SortedMap[IpResourceRange, Ipv6Record]
) {

 def fixIana: Records = {
   def fix[A <: Record : Updates]: A => A = (rec: A) => rec.status match {
     case IETF => rec.oid_(IETF).ext_(IANA)
     case IANA => rec.oid_(IANA).status_(ASSIGNED).ext_(IANA)
     case _ => rec.ext_(IANA)
   }
    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }

  // Reserved and available records normally have neither country code or date, when combined it will be filled with ZZ and today's date.
  def fixRIRs: Records = {
    def fix[A <: Record: Updates]: A => A = (rec: A) => rec.status match {
      case RESERVED | AVAILABLE => rec.date_(TODAY).cc_(DEFAULT_CC)
      case ALLOCATED => rec.status_(ASSIGNED)
      case _ => rec
    }
    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }


  override def toString: String =
    s"""Source: $source \nHeader: ${header.mkString(",")} \nSummaries: \n${summaries
      .map(_.mkString(","))
      .mkString("\n")}\n\n"""

  // Map values wrapper
  def asn_(f: AsnRecord => AsnRecord)(implicit ev: Updates[AsnRecord]): Records =
    this.copy(asn = this.asn.mapValues(f))
  def ipv4_(f: Ipv4Record => Ipv4Record)(implicit ev: Updates[Ipv4Record]): Records =
    this.copy(ipv4 = this.ipv4.mapValues(f))
  def ipv6_(f: Ipv6Record => Ipv6Record)(implicit ev: Updates[Ipv6Record]): Records =
    this.copy(ipv6 = this.ipv6.mapValues(f))

}
