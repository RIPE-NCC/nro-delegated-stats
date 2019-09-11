package net.ripe.rpki.nro

import net.ripe.ipresource.IpResource
import net.ripe.rpki.nro.Defs.{ASN, DEFAULT_CC, IANA, IANAPOOL, IPV4, IPV4_IANA_POOL_DATE, IPV6, TODAY}
import org.scalatest.FlatSpec

class RecordTest extends FlatSpec {

  "Iana pool calculation" should "work for Asn" in {
      assert(AsnRecord.ianapool(IpResource.parse("AS1000-AS2000")) ==
        AsnRecord( IANA, DEFAULT_CC, ASN, "1000", "1001" + "", TODAY, IANAPOOL, "", IANA))
  }

  it should "work for Ipv4" in {
    assert(Ipv4Record.ianapool(IpResource.parse("0.0.0.0/24")) ==
      Ipv4Record( IANA, DEFAULT_CC, IPV4, "0.0.0.0", "256", IPV4_IANA_POOL_DATE, IANAPOOL, "", IANA))
  }

  it should "work for Ipv6" in {
    assert(Ipv6Record.ianapool(IpResource.parse("::/24")) ==
      Ipv6Record(IANA, DEFAULT_CC, IPV6, "::", "24", TODAY, IANAPOOL, "", IANA))
  }
}
