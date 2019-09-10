package net.ripe.rpki.nro

import org.scalatest.{FlatSpec, PropSpec}
import Merger._
import Defs._
import net.ripe.ipresource.{IpResource, IpResourceRange}

class MergerTest extends FlatSpec {

  "Removing half of ipv6" should "produce the other half" in {
    assert(substractSpace(ALL_IPV6, Set(IpResourceRange.parse("::/1"))).toSet == Set(IpResourceRange.parse("8000::/1")))
    assert(substractSpace(ALL_IPV4, Set(IpResourceRange.parse("0.0.0.0/1"))).toSet == Set(IpResourceRange.parse("128.0.0.0/1")))
  }

}
