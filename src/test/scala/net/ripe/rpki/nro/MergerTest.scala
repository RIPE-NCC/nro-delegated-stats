package net.ripe.rpki.nro

import net.ripe.rpki.nro.Ports._
import org.scalatest.FlatSpec
import Merger._
import net.ripe.rpki.nro.Defs.Line

class MergerTest extends FlatSpec with TestUtil {

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
    assert(conflicts.head.rirsInvolved == "apnic--ripencc")
    // last one prevails
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

    val apnic = parseFileAsRecords(getClass.getResource("/data/apnic").getFile)
    val afrinic = parseFileAsRecords(getClass.getResource("/data/afrinic").getFile)
    val arin = parseFileAsRecords(getClass.getResource("/data/arin").getFile)
    val afriaprin = parseFileAsRecords(getClass.getResource("/data/afriaprin").getFile)

    val rirs = Iterable(apnic, afrinic, arin, afriaprin).map(_.fixRIRs)

    val (asns, asnConflicts) = combineResources(rirs.map(_.asn))
    val (ipv4s, ipv4Conflicts) = combineResources(rirs.map(_.ipv4))
    val (ipv6s, ipv6Conflicts) = combineResources(rirs.map(_.ipv6))

    assert(asns.size == 300)
    assert(ipv4s.size == 300)
    assert(ipv6s.size == 300)

    assert(asnConflicts.size == 100)
    assert(ipv4Conflicts.size == 100)
    assert(ipv6Conflicts.size == 100)

    // ASN conflicts are all between afrinic and afriaprin
    assert(asnConflicts.filter(_.rirsInvolved == "afrinic--afriaprin") == asnConflicts)

    // IPV4 conflicts are all between apnic and afriaprin
    assert(ipv4Conflicts.filter(_.rirsInvolved == "apnic--afriaprin") == ipv4Conflicts)

    // IPV4 conflicts are all between arin and afriaprin
    assert(ipv6Conflicts.filter(_.rirsInvolved == "arin--afriaprin") == ipv6Conflicts)
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
                  previousLines: String = "") = {

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