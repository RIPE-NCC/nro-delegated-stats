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

    // Whole global unicast 2000::/3, to be excluded
    val globalUnicastV6 = toRecords(List(List("iana", "ZZ", "ipv6") ++ toPrefixLength("2000::/3") ++ List("1990", "ietf")))
    // Fist /16 of global unicast, to be included
    val first16Unicast = toRecords(List(List("iana", "ZZ", "ipv6") ++ toPrefixLength("2000::/16") ++ List(IPV6_IANA_POOL_DATE, "reserved", "ietf", "iana")))

    // Recovered but not reallocated, to be excluded.
    val recoveredButNotReallocated = toRecords(fetchIpv4Reallocated(ianaOrgFileURL(IPV4_RECOVERED_SPACE)))
    // Reallocated assigned to be included
    val reallocatedAssigned: Records = reallocatedAndSpecialRegistries()

    val aggregatedIanaSpaces = allIanaAddressSpace
      .substract(globalUnicastV6).append(first16Unicast)
      .substract(recoveredButNotReallocated).append(reallocatedAssigned)

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

  def reallocatedAndSpecialRegistries(): Records = {

    // Recovered and reallocated, we need this.
    val ipv4Reallocated = fetchIpv4Reallocated(ianaOrgFileURL(IPV4_REALLOCATED_SPACE))

    // Special registry with special treatments inside.
    val ipv4SpecialRegistry = fetchIpv4SpecialRegs(ianaOrgFileURL(IPV4_SPECIAL_REGISTRY))

    val ipv6SpecialRegistry = fetchIpv6SpecialRegs(ianaOrgFileURL(IPV6_SPECIAL_REGISTRY))

    val asnSpecialRegistry = fetchAsnSpecialRegs(ianaOrgFileURL(ASN_SPECIAL_REGISTRY))

    logger.info("Fetch ipv6 unicast space, returning only those for RIRs")

    val unicastAssignmentV6 = toRecords(fetchIpv6(ianaOrgFileURL(IPV6_UNICAST_ASSIGNMENT)))
      .append(toRecords(ipv6SpecialRegistry))
      .append(toRecords(asnSpecialRegistry))
      .sorted()

    val reallocatedSpecial = toRecords(ipv4Reallocated ++ ipv4SpecialRegistry)
    reallocatedSpecial.append(unicastAssignmentV6)
  }

}
