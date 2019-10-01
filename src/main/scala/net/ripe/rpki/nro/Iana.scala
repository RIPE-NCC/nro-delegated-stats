package net.ripe.rpki.nro

import net.ripe.commons.ip._
import net.ripe.ipresource._
import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Record.length

import scala.jdk.CollectionConverters._
import scala.collection.{Iterable, Iterator}
import net.ripe.rpki.nro.Settings._

object Iana {

  def subtractRanges(slash0: IpResource, used : Iterable[IpResource]): Iterator[IpResource] = {
    val allSpace = new IpResourceSet(slash0)
    used.foreach(allSpace.remove)
    allSpace.iterator.asScala
  }

  // Filter for iana data not allocated for RIRs
  // Note: iana.status is actually registry
  def filterNonRIRs(iana: Records): (List[Record], List[Record], List[Record]) = {

    val nonRirData: Record => Boolean = r => !RIRS.contains(r.status)

    // Non RIRs a.k.a IETF reserved data from IANA is combined without modification
    (
      iana.asn.filter(nonRirData),
      iana.ipv4.filter(nonRirData),
      iana.ipv6.filter(nonRirData)
    )
  }

  def ianaPools(usedAsns: Iterable[IpResource],
                usedIpv4s: Iterable[IpResource],
                usedIpv6s: Iterable[IpResource]):
      (List[Record], List[Record], List[Record]) = {

    val asn  = subtractRanges(ALL_ASNS, usedAsns) .map(asnPool).toList
    val ipv4 = subtractRanges(ALL_IPV4, usedIpv4s).map(ipv4Pool).toList

    // Ipv6 needs to adjusted/split to bit boundary using other library (commons ip math)
    // To String and parse are converting between these ip libraries
    val ipv6 = subtractRanges(ALL_IPV6, usedIpv6s).map(_.toString)
      .flatMap(s => Ipv6Range.parse(s).splitToPrefixes.asScala)
      .map(a => IpResourceRange.parse(a.toString))
      .map(ipv6Pool).toList
    (asn, ipv4, ipv6)
  }

  def ipv4Pool(ipv4: IpResource) =
    Ipv4Record( IANA, DEFAULT_CC, IPV4, s"${ipv4.getStart}", s"${length(ipv4) }", IPV4_IANA_POOL_DATE, IANAPOOL, "", IANA)

  def asnPool(asn: IpResource) =
    AsnRecord( IANA, DEFAULT_CC, ASN, s"${asn.getStart.getValue}", s"${length(asn)}", TODAY, IANAPOOL, "", IANA)

  def ipv6Pool(ipv6: IpResource): Ipv6Record = {
    val Array(start, prefix) = ipv6.toString.split("/")
    Ipv6Record(IANA, DEFAULT_CC, IPV6, start, prefix, TODAY, IANAPOOL, "", IANA)
  }
}
