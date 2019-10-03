package net.ripe.rpki.nro.main

import net.ripe.rpki.nro.service.Ports.parseRecordFile
import net.ripe.rpki.nro.{Logging, TestUtil}
import org.scalatest.FlatSpec

class StatsTest extends FlatSpec with Stats with TestUtil with Logging {

  "Stats " should "produces something" in {
    val iana = parseRecordFile(getResourceFile("/data/iana")).fixIana
    val apnic = parseRecordFile(getResourceFile("/data/apnic"))

    val (results, mergedResults, currentConflicts, unclaimed, overclaimed) = process(Iterable(apnic), iana, List())

    assert(results.asn.nonEmpty)
    assert(mergedResults.asn.nonEmpty)
    assert(currentConflicts.isEmpty)
    assert(unclaimed.asn.nonEmpty)
    assert(overclaimed.asn.nonEmpty)
  }

}