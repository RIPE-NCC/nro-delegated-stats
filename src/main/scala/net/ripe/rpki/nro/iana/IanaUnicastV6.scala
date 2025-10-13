package net.ripe.rpki.nro.iana

import net.ripe.rpki.nro.Const.IPV6_IANA_POOL_DATE
import net.ripe.rpki.nro.iana.IanaGenerator.toPrefixLength
import net.ripe.rpki.nro.service.Ports.toRecords

object IanaUnicastV6 {
  // Whole global unicast 2000::/3, to be excluded, so that it will be marked as available in the end (not reserved ietf) when no other iana files allocate the space.
  val GLOBAL_UNICAST_V6 =
    toRecords(List(List("iana", "ZZ", "ipv6") ++
      toPrefixLength("2000::/3") ++
      List(IPV6_IANA_POOL_DATE, "available", "iana", "iana")))

  // Except for the first /16 of global unicast, to be included as reserved for IETF
  val FIRST_SLASH_16_UNICAST_V6 =
    toRecords(List(List("iana", "ZZ", "ipv6") ++
      toPrefixLength("2000::/16") ++
      List(IPV6_IANA_POOL_DATE, "reserved", "ietf", "iana")))

}

