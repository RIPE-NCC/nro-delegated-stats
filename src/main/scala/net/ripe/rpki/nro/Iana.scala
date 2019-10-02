package net.ripe.rpki.nro

import net.ripe.commons.ip._
import net.ripe.rpki.nro.Const._
import net.ripe.rpki.nro.Settings._

import scala.jdk.CollectionConverters._

object Iana {

  def ianaPools(records: Records): Records = {
    val (asn, ipv4, ipv6) = ianaPools(
      records.asn.map(_.range),
      records.ipv4.map(_.range),
      records.ipv6.map(_.range)
    )
    Records(asn, ipv4, ipv6)
  }

  def ianaPools(usedAsns: List[RecordRange],
                usedIpv4s: List[RecordRange],
                usedIpv6s: List[RecordRange]): (List[Record], List[Record], List[Record]) = (
      subtractRanges(ALL_ASNS, usedAsns) .map(asnPool),
      subtractRanges(ALL_IPV4, usedIpv4s).map(ipv4Pool),
      subtractRanges(ALL_IPV6, usedIpv6s).flatMap(_.ipv6.splitToPrefixes()
        .asScala.map(RecordRange.from)).map(ipv6Pool)
  )

  def ipv4Pool(ipv4: RecordRange): Ipv4Record = Ipv4Record(IANA, DEFAULT_CC, IPV4, s"${Ipv4.of(ipv4.rStart)}", s"${ipv4.length}", IPV4_IANA_POOL_DATE, IANAPOOL, "", IANA)

  def asnPool(asn: RecordRange): AsnRecord = AsnRecord(IANA, DEFAULT_CC, ASN, s"${asn.rStart}", s"${asn.length}", TODAY, IANAPOOL, "", IANA)

  def ipv6Pool(ipv6: RecordRange): Ipv6Record = {
    val Array(start, prefix) = Ipv6Range.from(ipv6.rStart).to(ipv6.rEnd).toStringInCidrNotation.split("/")
    Ipv6Record(IANA, DEFAULT_CC, IPV6, start, prefix, TODAY, IANAPOOL, "", IANA)
  }

  def subtractRanges(slash0: AsnRange, used: Iterable[RecordRange]): List[RecordRange] = {
    val allSpace = new SortedRangeSet[Asn, AsnRange]()
    allSpace.add(slash0)
    used.map(_.asn).foreach(allSpace.remove)
    allSpace.iterator.asScala.map(RecordRange.from).toList
  }

  def subtractRanges(slash0: Ipv4Range, used: Iterable[RecordRange]): List[RecordRange] = {
    val allSpace = new SortedRangeSet[Ipv4, Ipv4Range]()
    allSpace.add(slash0)
    used.map(_.ipv4).foreach(allSpace.remove)
    allSpace.iterator.asScala.map(RecordRange.from).toList
  }

  def subtractRanges(slash0: Ipv6Range, used: Iterable[RecordRange]): List[RecordRange] = {
    val allSpace = new SortedRangeSet[Ipv6, Ipv6Range]()
    allSpace.add(slash0)
    used.map(_.ipv6).foreach(allSpace.remove)
    allSpace.iterator.asScala.map(RecordRange.from).toList
  }
}
