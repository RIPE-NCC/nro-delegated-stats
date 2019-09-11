package net.ripe.rpki.nro

import net.ripe.ipresource.IpResource
import net.ripe.rpki.nro.Defs.{ALL_IPV4, ALL_IPV6}
import net.ripe.rpki.nro.Ports.parseFile
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

    assert(asn.keys.head  == IpResource.parse("AS2100000001-AS4200000000"))
    assert(ipv4.keys.head == IpResource.parse("128.0.0.0/1"))
    assert(ipv6.keys.head == IpResource.parse("8000::/1"))
  }

  "Filter non RIRs data from iana" should "give either ietf or iana status" in {
    val iana = parseFile(getClass.getResource("/data/iana").getFile)
    val (asn, ipv4, ipv6) = Iana.filterNonRIRs(iana.fixIana)

    // Iana test file is simplified only to contain one non rir data for each type
    assert(asn.size  == 1)
    assert(ipv4.size == 1)
    assert(ipv6.size == 1)
  }
}
