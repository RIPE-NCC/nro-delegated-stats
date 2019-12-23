package net.ripe.rpki.nro.main

import net.ripe.commons.ip.{AsnRange, Ipv4Range, Ipv6Range}
import net.ripe.rpki.nro.{Configs, iana}
import net.ripe.rpki.nro.Const._
import net.ripe.rpki.nro.iana.IanaPools
import net.ripe.rpki.nro.model.{AsnRecord, Ipv4Record, Ipv6Record, Record, RecordRange, Records, Stat}
import net.ripe.rpki.nro.service.Ports.parseRecordFile
import org.scalatest.FlatSpec

class IanaPoolsTest extends FlatSpec {

  "Removing half of ipv6" should "produce the other half" in {
    val actual  = IanaPools(Records(List(), List(), List(IanaPools.ipv6Pool(RecordRange.from(Ipv6Range.parse("::/1"))))))
    val expected = Iterator(RecordRange.from(Ipv6Range.parse("8000::/1"))).toList.map(_.ipv6)
    assert(actual.ipv6.map(_.range.ipv6) === expected)
  }

  "Removing half of ipv4 " should "produce the other half" in {
    val actual = iana.IanaPools(Records(List(), List(IanaPools.ipv6Pool(RecordRange.from(Ipv4Range.parse("0.0.0.0/1")))), List()))
    val expected = Iterator(RecordRange.from(Ipv4Range.parse("128.0.0.0/1"))).toList.map(_.ipv4)
    assert(actual.ipv4.map(_.range.ipv4) === expected)
  }

  "Calculating iana pools " should " when half of the internet is used should give the other half " in {

    val pool = iana.IanaPools(Records(
      List(IanaPools.asnPool(RecordRange.from(AsnRange.parse("AS0-AS2100000000")))),
      List(IanaPools.ipv4Pool(RecordRange.from(Ipv4Range.parse("0.0.0.0/1")))),
      List(IanaPools.ipv6Pool(RecordRange.from(Ipv6Range.parse("::/1"))))))

    assert(pool.asn.head.range  == RecordRange.from(AsnRange.parse("AS2100000001-AS4200000000")))
    assert(pool.ipv4.head.range == RecordRange.from(Ipv4Range.parse("128.0.0.0/1")))
    assert(pool.ipv6.head.range == RecordRange.from(Ipv6Range.parse("8000::/1")))
  }

  "Iana pools" should "be empty when calculated from appending original + calculated pools" in {

    val original: Records = parseRecordFile(getClass.getResource("/data/iana").getFile)
    val pools: Records = IanaPools.apply(original)

    val all: Records = original.append(pools).sorted()

    val empty = IanaPools.apply(all)
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
    assert(IanaPools.asnPool(RecordRange.from(AsnRange.parse("AS1000-AS2000"))) ==
      AsnRecord( Stat(IANA, DEFAULT_CC, ASN, "1000", "1001" + "", Configs.config.CURRENT_DAY, IANAPOOL, "", IANA)))
  }

  it should "work for Ipv4" in {
    assert(IanaPools.ipv4Pool(RecordRange.from(Ipv4Range.parse("0.0.0.0/24"))) ==
      Ipv4Record( Stat(IANA, DEFAULT_CC, IPV4, "0.0.0.0", "256", IPV4_IANA_POOL_DATE, IANAPOOL, "", IANA)))
  }

  it should "work for Ipv6" in {
    assert(IanaPools.ipv6Pool(RecordRange.from(Ipv6Range.parse("::/24"))) ==
      Ipv6Record(Stat(IANA, DEFAULT_CC, IPV6, "::", "24", Configs.config.CURRENT_DAY, IANAPOOL, "", IANA)))
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
