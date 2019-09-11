package net.ripe.rpki.nro

import net.ripe.commons.ip._
import net.ripe.ipresource._
import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Record.rangeLen

import scala.collection.JavaConverters._
import scala.collection.{Iterable, Iterator, SortedMap}

object Iana {

  def subtractRanges(slash0: IpResource, used : Iterable[IpResource]): Iterator[IpResource] = {
    val allSpace = new IpResourceSet(slash0)
    used.foreach(allSpace.remove)
    allSpace.iterator.asScala
  }

  // Filter for iana data not allocated for RIRs
  // Note: iana.status is actually registry
  def filterNonRIRs(iana: Records): (SortedMap[IpResource, AsnRecord], SortedMap[IpResource, Ipv4Record], SortedMap[IpResource, Ipv6Record]) = {

    val nonRirData: ((IpResource, Record)) => Boolean = {
      case (_, v) => !RIRS.contains(v.status)
    }

    // Non RIRs a.k.a IETF reserved data from IANA is combined without modification
    (iana.asn.filter(nonRirData), iana.ipv4.filter(nonRirData), iana.ipv6.filter(nonRirData))
  }

  def ianaPools(asns: Iterable[IpResource], ipv4s: Iterable[IpResource], ipv6s: Iterable[IpResource]): (Map[IpResource, AsnRecord], Map[IpResource, Ipv4Record], Map[IpResourceRange, Ipv6Record]) = {

    val asn  = subtractRanges(ALL_ASNS, asns).map(asn => asn -> asnPool(asn)).toMap
    val ipv4 = subtractRanges(ALL_IPV4, ipv4s).map(ipv4 => ipv4 -> ipv4Pool(ipv4)).toMap

    // Ipv6 needs to adjusted/split to bit boundary using other library (commons ip math)
    // To String and parse are converting between these ip libraries
    val ipv6 = subtractRanges(ALL_IPV6, ipv6s).map(_.toString)
      .flatMap(s => Ipv6Range.parse(s).splitToPrefixes.asScala)
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> ipv6Pool(a))
      .toMap

    (asn, ipv4, ipv6)
  }

  // Ipv4 ianapool on Jeff's combined results always dated 20120801, Magic date, where is it from? The ASN and Ipv6 is dated TODAY
  def ipv4Pool(ipv4: IpResource) =
    Ipv4Record( IANA, DEFAULT_CC, IPV4, ipv4.getStart + "", rangeLen(ipv4) + "", IPV4_IANA_POOL_DATE, IANAPOOL, "", IANA)

  def asnPool(asn: IpResource) =
    AsnRecord( IANA, DEFAULT_CC, ASN, asn.getStart.getValue + "", rangeLen(asn) + "", TODAY, IANAPOOL, "", IANA)

  def ipv6Pool(ipv6: IpResource): Ipv6Record = {
    val Array(start, prefix) = ipv6.toString.split("/")
    Ipv6Record(IANA, DEFAULT_CC, IPV6, start, prefix, TODAY, IANAPOOL, "", IANA)
  }
}
