package net.ripe.rpki.nro

import net.ripe.ipresource.IpAddress
import org.scalatest.FlatSpec

class UtilTest extends FlatSpec {

  behavior of "UtilTest"

  it should "validate valid prefix " in {
    val start = IpAddress.parse("0:0:8::").getValue

    assert(Util.validIpv6  (start, 46))
    assert(Util.validIpv6  (start, 45))
    assert(!Util.validIpv6 (start, 44))
    assert(!Util.validIpv6 (start, 43))
  }

}
