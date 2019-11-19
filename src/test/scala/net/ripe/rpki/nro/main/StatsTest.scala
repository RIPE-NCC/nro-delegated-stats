package net.ripe.rpki.nro.main

import net.ripe.rpki.nro.service.Ports.parseRecordFile
import net.ripe.rpki.nro.{Logging, TestUtil}
import org.scalatest.FlatSpec

class StatsTest extends FlatSpec with Stats with TestUtil with Logging {

  "Stats sanity check " should "detects conflict, unclaimed, and claimed" in {
    val iana = parseRecordFile(getResourceFile("/data/iana")).fixIana
    val apnic = parseRecordFile(getResourceFile("/data/apnic")).fixRIRs
    val afrinic = parseRecordFile(getResourceFile("/data/afrinic")).fixRIRs
    val arin = parseRecordFile(getResourceFile("/data/arin")).fixRIRs
    val afriaprin = parseRecordFile(getResourceFile("/data/afriaprin")).fixRIRs

    val (results, mergedResults, currentConflicts, unclaimed, overclaimed) = process(Iterable(apnic, afrinic, arin, afriaprin), iana, None, List())

    assert(mergedResults.size <= results.size)
    assert(currentConflicts.size === 300)
    assert(unclaimed.size === 32)
    assert(overclaimed.size === 475)
  }

  it should "detect no conflict when using afriaprin as previous" in {

    val iana = parseRecordFile(getResourceFile("/data/iana")).fixIana
    val apnic = parseRecordFile(getResourceFile("/data/apnic")).fixRIRs
    val afrinic = parseRecordFile(getResourceFile("/data/afrinic")).fixRIRs
    val arin = parseRecordFile(getResourceFile("/data/arin")).fixRIRs
    val previous = parseRecordFile(getResourceFile("/data/previous")).fixRIRs

    val (results, mergedResults, currentConflicts, unclaimed, overclaimed) = process(Iterable(apnic, afrinic, arin), iana, Some(previous), List())

    assert(mergedResults.size <= results.size)
    assert(currentConflicts.isEmpty)
    assert(unclaimed.size === 32)
    assert(overclaimed.size === 475)

    // Assert that all unclaimed are now assigned and obtained from previous results.
    // No longer shown as available.
    assert(unclaimed.asn.map(_.status).forall(_ == "assigned"))
    assert(unclaimed.ipv4.map(_.status).forall(_ == "assigned"))

    // Previous data for non ARIN ipv6 are all assigned, so resolve to assigned.
    assert(unclaimed.ipv6.filter(_.registry != "arin").map(_.status).forall(_ == "assigned"))
    // Previous data for ARIN ipv6 is ianapool, now it will resolve to available
    assert(unclaimed.ipv6.filter(_.registry == "arin").map(_.status).forall(_ == "available"))
    
    

  }


}
