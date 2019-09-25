package net.ripe.rpki.nro

import net.ripe.ipresource.IpResource
import net.ripe.rpki.nro.Defs.{ALL_IPV4, ALL_IPV6, ASN, DEFAULT_CC, IANA, IANAPOOL, IPV4, IPV4_IANA_POOL_DATE, IPV6}
import net.ripe.rpki.nro.Ports.parseRecordFile
import net.ripe.rpki.nro.Settings._
import org.scalatest.FlatSpec

class IanaTest extends FlatSpec {

  "Removing half of ipv6" should "produce the other half" in {
    assert(Iana.subtractRanges(ALL_IPV6, Set(IpResource.parse("::/1"))).toSet == Set(IpResource.parse("8000::/1")))
    assert(Iana.subtractRanges(ALL_IPV4, Set(IpResource.parse("0.0.0.0/1"))).toSet == Set(IpResource.parse("128.0.0.0/1")))
  }

  "Calculating iana pools " should " when half of the internet is used should give the other half " in {

    val (asn, ipv4, ipv6) = Iana.ianaPools(
      Iterable(IpResource.parse("AS0-AS2100000000")),
      Iterable(IpResource.parse("0.0.0.0/1")),
      Iterable(IpResource.parse("::/1")))

    assert(asn.head.range  == IpResource.parse("AS2100000001-AS4200000000"))
    assert(ipv4.head.range == IpResource.parse("128.0.0.0/1"))
    assert(ipv6.head.range == IpResource.parse("8000::/1"))
  }

  "Filter non RIRs data from iana" should "give either ietf or iana status" in {
    val iana = parseRecordFile(getClass.getResource("/data/iana").getFile)
    val (asn, ipv4, ipv6) = Iana.filterNonRIRs(iana.fixIana)

    // Iana test file is simplified only to contain one non rir data for each type
    assert(asn.size  == 1)
    assert(ipv4.size == 1)
    assert(ipv6.size == 1)
  }

  "Iana pool calculation" should "work for Asn" in {
    assert(Iana.asnPool(IpResource.parse("AS1000-AS2000")) ==
      AsnRecord( IANA, DEFAULT_CC, ASN, "1000", "1001" + "", TODAY, IANAPOOL, "", IANA))
  }

  it should "work for Ipv4" in {
    assert(Iana.ipv4Pool(IpResource.parse("0.0.0.0/24")) ==
      Ipv4Record( IANA, DEFAULT_CC, IPV4, "0.0.0.0", "256", IPV4_IANA_POOL_DATE, IANAPOOL, "", IANA))
  }

  it should "work for Ipv6" in {
    assert(Iana.ipv6Pool(IpResource.parse("::/24")) ==
      Ipv6Record(IANA, DEFAULT_CC, IPV6, "::", "24", TODAY, IANAPOOL, "", IANA))
  }
}
