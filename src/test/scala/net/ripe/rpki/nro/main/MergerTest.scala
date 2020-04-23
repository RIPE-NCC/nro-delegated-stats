package net.ripe.rpki.nro.main

import net.ripe.rpki.nro.TestUtil
import net.ripe.rpki.nro.service.Ports._
import net.ripe.rpki.nro.model.{Conflict, Record, Records}
import org.scalatest.FlatSpec

class MergerTest extends FlatSpec with TestUtil with Merger {

  "Conflicts" should "be detected for first lines of these data " in {
    val ripe =
      """|ripencc|AU|ipv4|1.10.10.0|256|20110811|assigned|A9173591
         |ripencc|CN|ipv4|1.10.16.0|4096|20110412|allocated|A92319D5""".stripMargin

    val apnic =
      """|apnic|AU|ipv4|1.10.10.0|256|20110811|assigned|A9173591
         |apnic|CN|ipv4|1.10.11.0|256|20110414|allocated|A92E1062""".stripMargin

    val ripeRecords = toRecords(ripe)
    val apnicRecords = toRecords(apnic)

    val records = Iterable(apnicRecords, ripeRecords)
    val (merged, conflicts) = combineResources(records)


    assert(conflicts.size == 1)
    assert(merged.size == 3)
    assert(merged.head == ripeRecords.head)
  }

  it should "detect partial conflicts" in {
    val apnic =
      """|apnic|AU|ipv4|1.10.10.0|128|20110811|assigned|A9173591
         |apnic|CN|ipv4|1.10.11.0|256|20110414|allocated|A92E1062""".stripMargin

    val ripe =
      """|ripencc|AU|ipv4|1.10.10.0|256|20110811|assigned|A9173591
         |ripencc|CN|ipv4|1.10.16.0|4096|20110412|allocated|A92319D5""".stripMargin

    val ripeRecords = toRecords(ripe)
    val apnicRecords = toRecords(apnic)

    val records = Iterable(apnicRecords, ripeRecords)
    val (merged, conflicts) = combineResources(records)


    assert(conflicts.size == 1)
    assert(merged.size == 3)
    assert(merged.head == ripeRecords.head)
  }

  it should "be detected for afriaprin (fake RIR taking asn from afrinic, ipv4 from apnic, and ipv6 from arin)" in {

    val apnic = parseRecordFile(getResourceFile("/data/apnic"))
    val afrinic = parseRecordFile(getResourceFile("/data/afrinic"))
    val arin = parseRecordFile(getResourceFile("/data/arin"))
    val afriaprin = parseRecordFile(getResourceFile("/data/afriaprin"))

    val rirs: Iterable[Records] = Iterable(apnic, afrinic, arin, afriaprin).map(_.formatRIRs)

    val (result, conflicts) = combineRecords(rirs)

    assert(result.asn.size == 300)
    assert(result.ipv4.size == 300)
    assert(result.ipv6.size == 300)

    val asnConflicts = conflicts.filter(_.a.`type` == "asn")
    val ipv4Conflicts = conflicts.filter(_.a.`type` == "ipv4")
    val ipv6Conflicts = conflicts.filter(_.a.`type` == "ipv6")

    assert(asnConflicts.size == 100)
    assert(ipv4Conflicts.size == 100)
    assert(ipv6Conflicts.size == 100)

    // ASN conflicts are all between afrinic and afriaprin
    assert(asnConflicts.filter(_.rirsInvolved == "afrinic--afriaprin") == asnConflicts)

    // IPV4 conflicts are all between apnic and afriaprin
    assert(ipv4Conflicts.filter(_.rirsInvolved == "apnic--afriaprin") == ipv4Conflicts)

    // IPV4 conflicts are all between arin and afriaprin
    assert(ipv6Conflicts.filter(_.rirsInvolved == "arin--afriaprin") == ipv6Conflicts)

    val merged = mergeRecords(result)
    assert(merged.asn.size == 295)
    assert(merged.ipv4.size == 242)
    assert(merged.ipv6.size == 300)
  }

  "Merge siblings" should "merge adjacent asn entries" in {
    val splitted =
      """|arin|ZZ|asn|100000|1|20190815|assigned|8db2e0d637d4b1e9c912fa1832eeb396|e-stats
         |arin|ZZ|asn|397837|1|20190815|assigned|8db2e0d637d4b1e9c912fa1832eeb396|e-stats
         |arin|ZZ|asn|397838|1|20190815|assigned|8db2e0d637d4b1e9c912fa1832eeb396|e-stats
         |arin|ZZ|asn|397839|1|20190815|assigned|8db2e0d637d4b1e9c912fa1832eeb396|e-stats""".stripMargin

    val merged =
      """|arin|ZZ|asn|100000|1|20190815|assigned|8db2e0d637d4b1e9c912fa1832eeb396|e-stats
         |arin|ZZ|asn|397837|3|20190815|assigned|8db2e0d637d4b1e9c912fa1832eeb396|e-stats""".stripMargin

    val original = toRecords(splitted)
    val expected = toRecords(merged)

    assert(mergeSiblings(original) == expected)
  }

  it should "merge adjacent ipv4 entries" in {
    val splitted =
      """|apnic|ZZ|ipv4|0.0.0.0|256|20110412|assigned|A92319D5|e-stats
         |apnic|ZZ|ipv4|1.1.9.0|256|20110412|assigned|A92319D5|e-stats
         |apnic|ZZ|ipv4|1.1.10.0|512|20110412|assigned|A92319D5|e-stats
         |apnic|ZZ|ipv4|1.1.12.0|1024|20110412|assigned|A92319D5|e-stats
         |apnic|ZZ|ipv4|1.1.16.0|4096|20110412|assigned|A92319D5|e-stats
         |apnic|ZZ|ipv4|1.1.32.0|8192|20110412|assigned|A92319D5|e-stats""".stripMargin

    val merged =
      """|apnic|ZZ|ipv4|0.0.0.0|256|20110412|assigned|A92319D5|e-stats
         |apnic|ZZ|ipv4|1.1.9.0|14080|20110412|assigned|A92319D5|e-stats""".stripMargin

    val original = toRecords(splitted)
    val expected = toRecords(merged)

    assert(mergeSiblings(original) == expected)
  }

  it should "not merge adjacent ipv6 entries" in {
    val splitted =
      """|afrinic|ZZ|ipv6|2c0f:ea58::|32|20190910|assigned|F36D32D1|e-stats
         |afrinic|ZZ|ipv6|2c0f:ea59::|32|20190911|reserved||e-stats
         |afrinic|ZZ|ipv6|2c0f:ea5a::|31|20190911|reserved||e-stats
         |afrinic|ZZ|ipv6|2c0f:ea5c::|30|20190911|reserved||e-stats""".stripMargin

    val original = toRecords(splitted)

    assert(mergeSiblings(original) == original)
  }

  it should "performs multiple allowed merge if necessary" in {
    val ipv6s =
     """|arin|US|ipv6|2001:500:60::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:61::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:62::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:63::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:64::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:65::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:66::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:67::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:68::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:69::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:6a::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:6b::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:6c::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:6d::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:6e::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:6f::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:70::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:71::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:72::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:73::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:74::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:75::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:76::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:77::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:78::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:79::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:7a::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
        |arin|US|ipv6|2001:500:7b::|48|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats""".stripMargin

    val expected =
      """|arin|US|ipv6|2001:500:60::|44|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
         |arin|US|ipv6|2001:500:70::|45|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats
         |arin|US|ipv6|2001:500:78::|46|20090109|assigned|bfbf7b5fb987bbec05323e88fde8a507|e-stats""".stripMargin
    assert(mergeSiblings(toRecords(ipv6s))== toRecords(expected))
  }

  it should " split non overlapping ranges when merging for asn   " in {

    val ripes = List(
      record("ripencc", "asn", "11", "5"),
      record("ripencc", "asn", "21", "5"),
      record("ripencc", "asn", "31", "5"),
      record("ripencc", "asn", "41", "5"),
      record("ripencc", "asn", "51", "5"),
      record("ripencc", "asn", "61", "5"),
    )

    val apnic = List(
      record("apnic", "asn", "11", "5"),
      record("apnic", "asn", "21", "3"),
      record("apnic", "asn", "31", "7"),
      record("apnic", "asn", "42", "2"),
      record("apnic", "asn", "52", "4"),
      record("apnic", "asn", "64", "7"),
    )

    val (merged, conflicts) = combineResources(Iterable(ripes, apnic), List())
    val expectedMerge = List(
      record("apnic", "asn", "11", "5"),
      record("apnic", "asn", "21", "3"),
      record("ripencc", "asn", "24", "2"),
      record("apnic", "asn", "31", "7"),
      record("ripencc", "asn", "41", "1"),
      record("apnic", "asn", "42", "2"),
      record("ripencc", "asn", "44", "2"),
      record("ripencc", "asn", "51", "1"),
      record("apnic", "asn", "52", "4"),
      record("ripencc", "asn", "61", "3"),
      record("apnic", "asn", "64", "7"),
    )
    val expectedConflict = ripes.zip(apnic).map { case (r, a) => Conflict(r, a) }

    assert(merged == expectedMerge)
    assert(conflicts == expectedConflict)
  }

  it should "resolve partial conflict with previous in the middle " in {
    val ripes = List(
      record("ripencc", "asn", "1", "10"),
    )

    val apnic = List(
      record("apnic", "asn", "6", "10"),
    )

    val previous = List(
      record("apnic", "asn", "1", "15"),
    )

    val (merged, conflicts) = combineResources(Iterable(ripes, apnic), previous)
    val expectedMerge = List(
      record("ripencc", "asn", "1", "5"),
      record("apnic", "asn", "6", "10")
    )
    val expectedConflict = ripes.zip(apnic).map { case (r, a) => Conflict(r, a) }

    assert(merged == expectedMerge)
    assert(conflicts == expectedConflict)
  }

  it should " resolve conflict based on previous records for ASN" in {

    val afrinic = record("afrinic", "asn", "1", "100")
    val apnic = record("apnic", "asn", "1", "10")

    val previous = record("afrinic", "asn", "1", "100")

    val (merged, conflicts) = combineResources(Iterable(List(afrinic), List(apnic)), List(previous))

    val mergedSiblings = mergeSiblings(merged)

    val expectedMerge = List(previous)
    assert(mergedSiblings == expectedMerge)
    assert(conflicts == Conflict(afrinic, apnic) :: Nil)
  }

  it should " split non overlapping ranges when merging for ipv6   " in {

    val afrinic = record("afrinic", "ipv6", "2001:db8::", "64")
    val apnic = record("apnic", "ipv6", "2001:db8::", "68")

    val previous = record("afrinic", "ipv6", "2001:db8::", "64")

    val (merged, conflicts) = combineResources(Iterable(List(afrinic), List(apnic)), List(previous))

    val mergedSiblings = mergeSiblings(merged)

    val expectedMerge = List(previous)
    assert(mergedSiblings == expectedMerge)
    assert(conflicts == Conflict(afrinic, apnic) :: Nil)
  }

  it should " split non overlapping ranges when merging for ipv4   " in {

    val afrinic = record("afrinic", "ipv4", "1.1.1.0", "64")
    val apnic = record("apnic", "ipv4", "1.1.1.32", "64")


    val previous = record("apnic", "ipv4", "1.1.1.32", "64")

    val (merged, conflicts) = combineResources(Iterable(List(afrinic), List(apnic)), List(previous))

    val expectedMerge = record("afrinic", "ipv4", "1.1.1.0", "32") :: previous :: Nil

    assert(merged == expectedMerge)
    assert(conflicts == Conflict(afrinic, apnic) :: Nil)
  }


  def combineTest(olderLines: String,
                  newerLines: String,
                  previousLines: String = ""): (List[Record], List[Conflict]) = {

    val olderRecords = toRecords(olderLines)
    val newerRecords = toRecords(newerLines)
    val previousRecords = if (previousLines.nonEmpty) {
      toRecords(previousLines)
    } else {
      List()
    }
    val records = Iterable(olderRecords, newerRecords)
    combineResources(records, previousRecords)
  }

  def record(rir: String, resourceType: String, start: String, length: String): Record = {
    val line = s"""$rir|ZZ|$resourceType|$start|$length|20110811|assigned|ID"""
    toRecords(line).head
  }
}
