package net.ripe.rpki.nro

import net.ripe.commons.ip.Ipv6Range
import net.ripe.ipresource.{IpResource, IpResourceRange, IpResourceSet}
import net.ripe.rpki.nro.Defs.{ALL_ASNS, ALL_IPV4, ALL_IPV6, RIRS}

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

    val asnPool = subtractRanges(ALL_ASNS, asns).map(asn => asn -> AsnRecord.ianapool(asn)).toMap
    val ipv4pool = subtractRanges(ALL_IPV4, ipv4s).map(ipv4 => ipv4 -> Ipv4Record.ianapool(ipv4)).toMap

    // Ipv6 needs to adjusted/split to bit boundary using other library (commons ip math)
    // To String and parse are converting between these ip libraries
    val ipv6pool = subtractRanges(ALL_IPV6, ipv6s).map(_.toString)
      .flatMap(s => Ipv6Range.parse(s).splitToPrefixes.asScala)
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> Ipv6Record.ianapool(a))
      .toMap

    (asnPool, ipv4pool, ipv6pool)
  }
}
