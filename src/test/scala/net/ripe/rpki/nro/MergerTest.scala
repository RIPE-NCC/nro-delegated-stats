package net.ripe.rpki.nro

import net.ripe.rpki.nro.Merger._
import net.ripe.rpki.nro.Ports._
import org.scalatest.FlatSpec

class MergerTest extends FlatSpec {

  "Conflicts" should "be detected for first lines of these data " in {
    val apnic =
      """|apnic|AU|ipv4|1.10.10.0|256|20110811|assigned|A9173591
         |apnic|CN|ipv4|1.10.11.0|256|20110414|allocated|A92E1062""".stripMargin

    val ripe =
      """|ripencc|AU|ipv4|1.10.10.0|256|20110811|assigned|A9173591
         |ripencc|CN|ipv4|1.10.16.0|4096|20110412|allocated|A92319D5""".stripMargin

    val apnicRecords = toSortedRecordMap(parseLines(apnic.split("\n").toList), Ipv4Record.apply)
    val ripeRecords = toSortedRecordMap(parseLines(ripe.split("\n").toList), Ipv4Record.apply)
    val records = Iterable(apnicRecords, ripeRecords)
    val (merged, conflicts) = mergeAndDetectConflicts(records.par)

    merged.values.foreach(println)

    conflicts.foreach(println)
    assert(conflicts.size == 1)
  }

  "Conflicts" should "be detected for afriaprin (fake RIR taking asn from afrinic, ipv4 from apnic, and ipv6 from arin)" in {

    val apnic = parseFile(getClass.getResource("/data/apnic").getFile)
    val afrinic = parseFile(getClass.getResource("/data/afrinic").getFile)
    val arin = parseFile(getClass.getResource("/data/arin").getFile)
    val afriaprin = parseFile(getClass.getResource("/data/afriaprin").getFile)

    val rirs = Iterable(apnic, afrinic, arin, afriaprin).par.map(_.fixRIRs)

    val (asns, asnConflicts) = mergeAndDetectConflicts(rirs.map(_.asn))
    val (ipv4s, ipv4Conflicts) = mergeAndDetectConflicts(rirs.map(_.ipv4))
    val (ipv6s, ipv6Conflicts) = mergeAndDetectConflicts(rirs.map(_.ipv6))

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
}
