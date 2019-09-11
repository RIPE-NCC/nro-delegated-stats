package net.ripe.rpki.nro

import net.ripe.rpki.nro.Ports._
import org.scalatest.FlatSpec

class PortsTest extends FlatSpec {

  "Parsing delegated " should "work for partial apnic data" in {
    val apnic = parseFile(getClass.getResource("/data/apnic").getFile)
    assert(apnic.asn.size  == 100)
    assert(apnic.ipv4.size == 100)
    assert(apnic.ipv6.size == 100)
  }

  it should "work for partial iana data" in {
    val iana = parseFile(getClass.getResource("/data/iana").getFile)
    assert(iana.asn.size  == 6)
    assert(iana.ipv4.size == 6)
    assert(iana.ipv6.size == 6)
  }
}
