package net.ripe.rpki.nro

import net.ripe.ipresource.IpResource
import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Merger._
import net.ripe.rpki.nro.Ports._
import org.scalatest.FlatSpec

class MergerTest extends FlatSpec {

  "Removing half of ipv6" should "produce the other half" in {
    assert(subtractRanges(ALL_IPV6, Set(IpResource.parse("::/1"))).toSet == Set(IpResource.parse("8000::/1")))
    assert(subtractRanges(ALL_IPV4, Set(IpResource.parse("0.0.0.0/1"))).toSet == Set(IpResource.parse("128.0.0.0/1")))
  }

  val apnic =
    """|apnic|AU|ipv4|1.10.10.0|256|20110811|assigned|A9173591
       |apnic|CN|ipv4|1.10.11.0|256|20110414|allocated|A92E1062""".stripMargin

  val ripe =
    """|ripencc|AU|ipv4|1.10.10.0|256|20110811|assigned|A9173591
       |ripencc|CN|ipv4|1.10.16.0|4096|20110412|allocated|A92319D5""".stripMargin

  "Conflict between ripe and apnic" should " be found " in {
      val apnicRecords = toSortedRecordMap(parseLines(apnic.split("\n").toList), Ipv4Record.apply)
      val ripeRecords = toSortedRecordMap(parseLines(ripe.split("\n").toList), Ipv4Record.apply)
      val records = Iterable(apnicRecords,ripeRecords)
      val (merged, conflicts) = combine(records.par)

      merged.values.foreach(println)

      conflicts.foreach(println)
      assert(conflicts.size == 1)

  }
}
