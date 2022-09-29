package net.ripe.rpki.nro.iana

import net.ripe.rpki.nro.Configs.ianaOrgFileURL
import net.ripe.rpki.nro.Const.{ASN16, ASN32, ASN_SPECIAL_REGISTRY, IPV4_ADDRESS_SPACE, IPV4_REALLOCATED_SPACE, IPV4_RECOVERED_SPACE, IPV4_SPECIAL_REGISTRY, IPV6_ADDRESS_SPACE, IPV6_IANA_POOL_DATE, IPV6_SPECIAL_REGISTRY, IPV6_UNICAST_ASSIGNMENT}
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.main.Merger
import net.ripe.rpki.nro.model.Records
import net.ripe.rpki.nro.service.Ports.toRecords

object IanaGenerator extends Merger with Logging with IanaParser {

  def processIanaRecords: Records = {

    // Fetch ASN, IPv4 and IPv6 address space
    val allIanaAddressSpace = fetchAllIanaAddressSpace()

    // Whole global unicast 2000::/3, to be excluded, so that it will be marked as available in the end (not reserved ietf) when no other iana files allocate the space.
    val globalUnicastV6 = toRecords(List(List("iana", "ZZ", "ipv6") ++ toPrefixLength("2000::/3") ++ List(IPV6_IANA_POOL_DATE, "available", "iana", "iana")))
    // Except for the first /16 of global unicast, to be included as reserved for IETF
    val first16Unicast = toRecords(List(List("iana", "ZZ", "ipv6") ++ toPrefixLength("2000::/16") ++ List(IPV6_IANA_POOL_DATE, "reserved", "ietf", "iana")))

    // Recovered but not reallocated, to be excluded.
    val recoveredV4 = toRecords(fetchIpv4Reallocated(ianaOrgFileURL(IPV4_RECOVERED_SPACE)))
    // Reallocated IPv4
    val reallocatedV4 = toRecords(fetchIpv4Reallocated(ianaOrgFileURL(IPV4_REALLOCATED_SPACE)))

    // Special registries
    val specialRegistries = fetchSpecialRegistries()

    logger.info("Fetch ipv6 unicast space, returning only those for RIRs")
    val unicastV6 = toRecords(fetchIpv6(ianaOrgFileURL(IPV6_UNICAST_ASSIGNMENT)))

    val aggregatedIanaSpaces = allIanaAddressSpace
      .substract(globalUnicastV6).append(first16Unicast)
      .substract(recoveredV4).append(reallocatedV4)
      .append(unicastV6)
      .append(specialRegistries)

    val available = IanaPools(aggregatedIanaSpaces,"available")
    val (combinedWithAvailableSpaces, _) = combineRecords(Seq(aggregatedIanaSpaces, available), alignIpv4 = true)

    combinedWithAvailableSpaces
  }

  def fetchAllIanaAddressSpace(): Records = {
    logger.info("Fetch ASN16")
    val asn16 = fetchAsn(ianaOrgFileURL(ASN16))

    logger.info("Fetch ASN32")
    // Careful, IANA 32 contains this entry for 16 that needs to be skipped: iana|ZZ|asn|0|65536 => Explains the tail
    val asn32 = fetchAsn(ianaOrgFileURL(ASN32)).tail

    logger.info("Fetch ipv4 address space")
    val ipv4 = fetchIpv4(ianaOrgFileURL(IPV4_ADDRESS_SPACE))

    logger.info("Fetch ipv6 address space")
    val ipv6 = fetchIpv6(ianaOrgFileURL(IPV6_ADDRESS_SPACE))

    toRecords(asn16 ++ asn32 ++ ipv4 ++ ipv6)
  }

  def fetchSpecialRegistries(): Records = {

    logger.info("Fetch ipv4 special registries")
    val ipv4SpecialRegistry = fetchIpv4SpecialRegs(ianaOrgFileURL(IPV4_SPECIAL_REGISTRY))

    logger.info("Fetch ipv6 special registries")
    val ipv6SpecialRegistry = fetchIpv6SpecialRegs(ianaOrgFileURL(IPV6_SPECIAL_REGISTRY))

    logger.info("Fetch asn special registries")
    val asnSpecialRegistry = fetchAsnSpecialRegs(ianaOrgFileURL(ASN_SPECIAL_REGISTRY))

    toRecords(ipv4SpecialRegistry ++ipv6SpecialRegistry++asnSpecialRegistry)
  }
}
