package net.ripe.rpki.nro 

import java.math.BigInteger
import net.ripe.ipresource.{IpAddress, IpResourceRange, IpResourceType}
import scala.collection.SortedMap

import Defs._
import Updates._

// Records holder for three type of records, contains some logic of fixing entries
case class Records(
    source: String,
    header: Line,
    summaries: List[Line],
    asn: SortedMap[IpResourceRange, AsnRecord],
    ipv4: SortedMap[IpResourceRange, Ipv4Record],
    ipv6: SortedMap[IpResourceRange, Ipv6Record]
) {


 // ext field will be adjusted according to the source. (what about oid?)
  def fixExt: Records = {
    val ext                               = if (source == "iana") "iana" else "e-stats"
    def fix[A <: Record: Updates]: A => A = _.ext_(ext)

    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }

  def fixIetfIana: Records = {
    def fix[A <: Record: Updates]: A => A = (a: A) => {
      if (a.status == "ietf") a.oid_("ietf").ext_("iana")
      else if (a.status == "iana") a.oid_("iana").status_("assigned").ext_("iana")
      else a
    }

    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }

  // Reserved and available records normally have neither country code or date, when combined it will be filled with ZZ and today's date.
  def fixReservedAvailable: Records = {
    def fix[A <: Record: Updates]: A => A =
      (a: A) =>
        if (a.status == "reserved" || a.status == "available") a.date_(TODAY).cc_("ZZ") else a

    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }

  // RIR's allocated are all converted to assigned when combined.
  def fixAllocated: Records = {
    def fix[A <: Record: Updates]: A => A =
      (a: A) => if (a.status == "allocated") a.status_("assigned") else a

    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }

  override def toString: String =
    s"""Source: $source \nHeader: ${header.mkString(",")} \nSummaries: \n${summaries
      .map(_.mkString(","))
      .mkString("\n")}\n\n"""

  // Map values wrapper
  def asn_(f: AsnRecord => AsnRecord)(implicit ev: Updates[AsnRecord]) =
    this.copy(asn = this.asn.mapValues(f))
  def ipv4_(f: Ipv4Record => Ipv4Record)(implicit ev: Updates[Ipv4Record]) =
    this.copy(ipv4 = this.ipv4.mapValues(f))
  def ipv6_(f: Ipv6Record => Ipv6Record)(implicit ev: Updates[Ipv6Record]) =
    this.copy(ipv6 = this.ipv6.mapValues(f))

}
