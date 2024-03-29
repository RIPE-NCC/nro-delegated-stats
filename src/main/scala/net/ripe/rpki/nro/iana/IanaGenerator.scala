package net.ripe.rpki.nro.iana

import net.ripe.rpki.nro.Configs.ianaOrgFileURL
import net.ripe.rpki.nro.Const.{ASN16, ASN32, ASN_SPECIAL_REGISTRY, IPV4_ADDRESS_SPACE, IPV4_REALLOCATED_SPACE, IPV4_RECOVERED_SPACE, IPV4_SPECIAL_REGISTRY, IPV6_ADDRESS_SPACE, IPV6_SPECIAL_REGISTRY, IPV6_UNICAST_ASSIGNMENT}
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.iana.IanaUnicastV6.{FIRST_SLASH_16_UNICAST_V6, GLOBAL_UNICAST_V6}
import net.ripe.rpki.nro.main.Merger
import net.ripe.rpki.nro.model.Records
import net.ripe.rpki.nro.service.Ports.toRecords

object IanaGenerator extends Merger with Logging with IanaParser {

  def processIanaRecords: Records = {

    // Fetch ASN, IPv4 and IPv6 address space
    val allIanaAddressSpace = fetchAllIanaAddressSpace()

    // Recovered but not reallocated, to be excluded.
    val recoveredV4 = toRecords(fetchIpv4Reallocated(ianaOrgFileURL(IPV4_RECOVERED_SPACE)))
    // Reallocated IPv4
    val reallocatedV4 = toRecords(fetchIpv4Reallocated(ianaOrgFileURL(IPV4_REALLOCATED_SPACE)))
    // Special registries
    val specialRegistries = fetchSpecialRegistries()

    logger.info("Fetch ipv6 unicast space, returning only those for RIRs")
    val unicastV6 = toRecords(fetchIpv6(ianaOrgFileURL(IPV6_UNICAST_ASSIGNMENT)))

    val aggregatedIanaSpaces = allIanaAddressSpace
      .substract(GLOBAL_UNICAST_V6).append(FIRST_SLASH_16_UNICAST_V6)
      .substract(recoveredV4).append(reallocatedV4)
      .append(unicastV6)
      .append(specialRegistries)

    val availableIana = IanaPools(aggregatedIanaSpaces, "available")

    aggregatedIanaSpaces.align(availableIana)
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

    toRecords(ipv4SpecialRegistry ++ ipv6SpecialRegistry ++ asnSpecialRegistry)
  }
}
