package net.ripe.rpki.nro

import net.ripe.commons.ip.{AsnRange, Ipv4Range, Ipv6Range}
import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Ports.parseRecordFile
import net.ripe.rpki.nro.Settings._
import org.scalatest.FlatSpec

class IanaTest extends FlatSpec {

  "Removing half of ipv6" should "produce the other half" in {
    val actual = Iana.subtractRanges(ALL_IPV6, Iterable(RecordRange.from(Ipv6Range.parse("::/1")))).toList.map(_.ipv6)
    val expected = Iterator(RecordRange.from(Ipv6Range.parse("8000::/1"))).toList.map(_.ipv6)
    assert(actual === expected)
  }

  "Removing half of ipv4 " should "produce the other half" in {
    val actual = Iana.subtractRanges(ALL_IPV4, Iterable(RecordRange.from(Ipv4Range.parse("0.0.0.0/1")))).toList.map(_.ipv4)
    val expected = Iterator(RecordRange.from(Ipv4Range.parse("128.0.0.0/1"))).toList.map(_.ipv4)
    assert(actual === expected)
  }

  "Calculating iana pools " should " when half of the internet is used should give the other half " in {

    val (asn, ipv4, ipv6) = Iana.ianaPools(
      List(RecordRange.from(AsnRange.parse("AS0-AS2100000000"))),
      List(RecordRange.from(Ipv4Range.parse("0.0.0.0/1"))),
      List(RecordRange.from(Ipv6Range.parse("::/1"))))

    assert(asn.head.range  == RecordRange.from(AsnRange.parse("AS2100000001-AS4200000000")))
    assert(ipv4.head.range == RecordRange.from(Ipv4Range.parse("128.0.0.0/1")))
    assert(ipv6.head.range == RecordRange.from(Ipv6Range.parse("8000::/1")))
  }

  "Iana pools" should "be empty when calculated from appending original + calculated pools" in {

    val original: Records = parseRecordFile(getClass.getResource("/data/iana").getFile)
    val pools: Records = Iana.ianaPools(original)

    val all: Records = original.append(pools).sorted()

    val empty = Iana.ianaPools(all)
    assert(empty.asn.isEmpty)
    assert(empty.ipv4.isEmpty)
    assert(empty.ipv6.isEmpty)
  }

  "Filter non RIRs data from iana" should "give either ietf or iana status" in {
    val isRirRecord: Record => Boolean = r => RIRS.contains(r.status)
    val iana: Records = parseRecordFile(getClass.getResource("/data/iana").getFile)
    val (rirs, nonRirs) = iana.fixIana.partition(isRirRecord)

    assert(nonRirs.asn.size  == 1)
    assert(nonRirs.ipv4.size == 1)
    assert(nonRirs.ipv6.size == 1)

    assert(rirs.asn.size  == 5)
    assert(rirs.ipv4.size == 5)
    assert(rirs.ipv6.size == 5)
  }

  "Iana pool calculation" should "work for Asn" in {
    assert(Iana.asnPool(RecordRange.from(AsnRange.parse("AS1000-AS2000"))) ==
      AsnRecord( IANA, DEFAULT_CC, ASN, "1000", "1001" + "", TODAY, IANAPOOL, "", IANA))
  }

  it should "work for Ipv4" in {
    assert(Iana.ipv4Pool(RecordRange.from(Ipv4Range.parse("0.0.0.0/24"))) ==
      Ipv4Record( IANA, DEFAULT_CC, IPV4, "0.0.0.0", "256", IPV4_IANA_POOL_DATE, IANAPOOL, "", IANA))
  }

  it should "work for Ipv6" in {
    assert(Iana.ipv6Pool(RecordRange.from(Ipv6Range.parse("::/24"))) ==
      Ipv6Record(IANA, DEFAULT_CC, IPV6, "::", "24", TODAY, IANAPOOL, "", IANA))
  }

  "Claims verification " should  "be empty in case of proper combination" in {
    val iana: Records = parseRecordFile(getClass.getResource("/data/claims/iana").getFile)
    val combined : Records = parseRecordFile(getClass.getResource("/data/claims/combined").getFile)

    val unclaimed = iana.substract(combined)
    val overclaimed = combined.substract(iana)

    assert(unclaimed.asn.isEmpty)
    assert(unclaimed.ipv4.isEmpty)
    assert(unclaimed.ipv6.isEmpty)

    assert(overclaimed.asn.isEmpty)
    assert(overclaimed.ipv4.isEmpty)
    assert(overclaimed.ipv6.isEmpty)

  }

  it should " detect overclaims " in {
    val iana: Records = parseRecordFile(getClass.getResource("/data/claims/iana").getFile)
    val combined : Records = parseRecordFile(getClass.getResource("/data/claims/combined-overclaims").getFile)

    val unclaimed = iana.substract(combined)
    val overclaimed = combined.substract(iana)

    assert(unclaimed.asn.isEmpty)
    assert(unclaimed.ipv4.isEmpty)
    assert(unclaimed.ipv6.isEmpty)

    // check and compare input files above last line for every resource type
    assert(overclaimed.asn.size  == 1 && overclaimed.asn. head.length == "10")
    assert(overclaimed.ipv4.size == 1 && overclaimed.ipv4.head.length == "512")
    assert(overclaimed.ipv6.size == 1 && overclaimed.ipv6.head.length == "23")

  }

  it should " detect unclaimed " in {
    val iana: Records = parseRecordFile(getClass.getResource("/data/claims/iana").getFile)
    val combined : Records = parseRecordFile(getClass.getResource("/data/claims/combined-unclaimed").getFile)

    val unclaimed = iana.substract(combined)
    val overclaimed = combined.substract(iana)

    // check and compare input files above last line for every resource type
    assert(unclaimed.asn.size  == 1 && unclaimed.asn.head.length  === "2")
    assert(unclaimed.ipv4.size == 1 && unclaimed.ipv4.head.length === "8388352")
    assert(unclaimed.ipv6.size == 1 && unclaimed.ipv6.head.length === "24")

    assert(overclaimed.asn.isEmpty)
    assert(overclaimed.ipv4.isEmpty)
    assert(overclaimed.ipv6.isEmpty)
  }
}
