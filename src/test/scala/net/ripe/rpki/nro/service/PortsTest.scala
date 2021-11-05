package net.ripe.rpki.nro.service

import java.io.File

import net.ripe.rpki.nro.TestUtil
import net.ripe.rpki.nro.service.Ports._
import net.ripe.rpki.nro.model.Conflict
import org.scalatest.FlatSpec

class PortsTest extends FlatSpec with TestUtil {

  "Parsing delegated " should "work for partial apnic data" in {
    val apnic = parseRecordFile(getResourceFile("/data/apnic"))
    assert(apnic.asn.size == 100)
    assert(apnic.ipv4.size == 100)
    assert(apnic.ipv6.size == 100)
  }

  it should "work for partial iana data" in {
    val iana = parseRecordFile(getResourceFile("/data/iana"))
    assert(iana.asn.size == 8)
    assert(iana.ipv4.size == 6)
    assert(iana.ipv6.size == 6)
  }

  it should "deserialize conflict " in {
    val fetchConflicts = readConflicts("https://ftp.ripe.net/pub/stats/ripencc/nro-stats/20210810/conflicts")
    assert(fetchConflicts.size == 7)
  }

  it should "deserialize empty conflicts" in {
    val noConflict = List[Conflict]()
    val fetched = readConflicts("https://ftp.ripe.net/pub/stats/ripencc/nro-stats/20210810/unclaimed")
    assert(fetched == noConflict)
  }

}
