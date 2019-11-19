package net.ripe.rpki.nro.main

import net.ripe.commons.ip.{Ipv4, Ipv6Range}
import net.ripe.rpki.nro.Const.{ALL_ASNS, ALL_IPV4, ALL_IPV6, ASN, DEFAULT_CC, IANA, IANAPOOL, IPV4, IPV4_IANA_POOL_DATE, IPV6}
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.model
import net.ripe.rpki.nro.model.{AsnRecord, Ipv4Record, Ipv6Record, RecordRange, Records, Stat}

object IanaPools {

  def apply(records: Records): Records = {
    Records(
      List(asnPool(RecordRange.from(ALL_ASNS))),
      List(ipv4Pool(RecordRange.from(ALL_IPV4))),
      List(ipv6Pool(RecordRange.from(ALL_IPV6))),
    ).substract(records)
  }

  def ipv4Pool(ipv4: RecordRange): Ipv4Record = Ipv4Record(Stat(IANA, DEFAULT_CC, IPV4, s"${Ipv4.of(ipv4.rStart)}", s"${ipv4.length}", IPV4_IANA_POOL_DATE, IANAPOOL, "", IANA))

  def asnPool(asn: RecordRange): AsnRecord = AsnRecord(Stat(IANA, DEFAULT_CC, ASN, s"${asn.rStart}", s"${asn.length}", config.CURRENT_DAY, IANAPOOL, "", IANA))

  def ipv6Pool(ipv6: RecordRange): Ipv6Record = {
    val Array(start, prefix) = Ipv6Range.from(ipv6.rStart).to(ipv6.rEnd).toStringInCidrNotation.split("/")
    Ipv6Record(model.Stat(IANA, DEFAULT_CC, IPV6, start, prefix, config.CURRENT_DAY, IANAPOOL, "", IANA))
  }
}
