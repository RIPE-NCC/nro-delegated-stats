package net.ripe.rpki.nro.iana

import net.ripe.commons.ip.{Ipv4, Ipv6Range}
import net.ripe.rpki.nro.Const.{ALL_ASNS, ALL_IPV4, ALL_IPV6, ASN, ASN_IANA_POOL_DATE, DEFAULT_CC, IANA, IANAPOOL, IPV4, IPV4_IANA_POOL_DATE, IPV6, IPV6_IANA_POOL_DATE}
import net.ripe.rpki.nro.model
import net.ripe.rpki.nro.model.{AsnRecord, Ipv4Record, Ipv6Record, RecordRange, Records, Stat}

object IanaPools {

  // Calculate what is available on iana pool after substracting from used records
  def apply(usedRecords: Records, status: String = IANAPOOL): Records = {
    Records(
      List(asnPool(RecordRange.from(ALL_ASNS), status)),
      List(ipv4Pool(RecordRange.from(ALL_IPV4), status)),
      List(ipv6Pool(RecordRange.from(ALL_IPV6), status)),
    ).substract(usedRecords)
  }

  def ipv4Pool(ipv4: RecordRange, status: String = IANAPOOL): Ipv4Record = Ipv4Record(Stat(IANA, DEFAULT_CC, IPV4, s"${Ipv4.of(ipv4.rStart)}", s"${ipv4.length}", IPV4_IANA_POOL_DATE, status, IANA, IANA))

  def asnPool(asn: RecordRange, status: String = IANAPOOL): AsnRecord = AsnRecord(Stat(IANA, DEFAULT_CC, ASN, s"${asn.rStart}", s"${asn.length}", ASN_IANA_POOL_DATE, status, IANA, IANA))

  def ipv6Pool(ipv6: RecordRange, status: String = IANAPOOL): Ipv6Record = {
    val Array(start, prefix) = Ipv6Range.from(ipv6.rStart).to(ipv6.rEnd).toStringInCidrNotation.split("/")
    Ipv6Record(model.Stat(IANA, DEFAULT_CC, IPV6, start, prefix, IPV6_IANA_POOL_DATE, status, IANA, IANA))
  }
}
