package net.ripe.rpki.nro.iana

import net.ripe.rpki.nro.Configs.ianaOrgFileURL
import net.ripe.rpki.nro.Const.{ASN16, ASN32, IPV4_ADDRESS_SPACE, IPV4_REALLOCATED_SPACE, IPV4_RECOVERED_SPACE, IPV4_SPECIAL_REGISTRY, IPV6_ADDRESS_SPACE, IPV6_SPECIAL_REGISTRY, IPV6_UNICAST_ASSIGNMENT}
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.main.Merger
import net.ripe.rpki.nro.model.Records
import net.ripe.rpki.nro.service.Ports.toRecords
import net.ripe.rpki.nro.service.Ports.writeRecords

object IanaGenerator extends Merger with Logging with IanaParser {

  def processIanaRecords: Records = {

    val ianaSpaceWithoutGlobalUnicastAndRecovered: Records = fetchAllIanaSpace()
      .substract(excludeGlobalUnicastAndRecovered())
      .append(includeFirst16OfGlobalUnicast())

    val reallocatedAssigned: Records = fetchUnicastAssignmentV6ReallocatedSpecialV4()

    val (aggregatedIanaResources, _) = combineRecords(Seq(ianaSpaceWithoutGlobalUnicastAndRecovered, reallocatedAssigned), Some(reallocatedAssigned), alignIpv4 = true)

    val available = IanaPools(aggregatedIanaResources,"available")

    val (combinedWithAvailableSpaces, _) = combineRecords(Seq(aggregatedIanaResources, available), alignIpv4=true)
    // For comparison purpose
    writeRecords(aggregatedIanaResources, "iana-own-generated", disclaimer=true)
    writeRecords(combinedWithAvailableSpaces, "iana-with-available", disclaimer=true)

    aggregatedIanaResources
  }

  def fetchAllIanaSpace(): Records = {
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

  private def excludeGlobalUnicastAndRecovered(): Records = {

    // Whole global unicast 2000::/33 minus 2000::/16 since we want to include the latter.
    val globalUnicastV6 = toRecords(List(List("iana", "ZZ", "ipv6") ++ toPrefixLength("2000::/3") ++ List("1990", "ietf")))

    // Recovered but not allocated
    val ipv4Recovered = toRecords(fetchIpv4Reallocated(ianaOrgFileURL(IPV4_RECOVERED_SPACE)))

    globalUnicastV6.append(ipv4Recovered)
  }

  private def includeFirst16OfGlobalUnicast(): Records = {
    // But include the first /16, don't really have explanation why.
    toRecords(List(List("iana", "ZZ", "ipv6") ++ toPrefixLength("2000::/16") ++ List("19960801", "reserved", "ietf", "iana")))
  }

  def fetchUnicastAssignmentV6ReallocatedSpecialV4(): Records = {

    // Recovered and reallocated, we need this.
    val ipv4Reallocated = fetchIpv4Reallocated(ianaOrgFileURL(IPV4_REALLOCATED_SPACE))

    // Special registry with special treatments inside.
    val ipv4SpecialRegistry = fetchIpv4SpecialRegs(ianaOrgFileURL(IPV4_SPECIAL_REGISTRY))

    val ipv6SpecialRegistry = fetchIpv6SpecialRegs(ianaOrgFileURL(IPV6_SPECIAL_REGISTRY))

    // RFC2928, without this it will be marked as IETF while geoff marked as IANA assignment, maybe I don' t need to do this.
    val ianaSlash23 = toRecords(List(List("iana", "ZZ", "ipv6") ++ toPrefixLength("2001::/23") ++ List("19960801", "reserved", "ietf", "iana")))

    logger.info("Fetch ipv6 unicast space, returning only those for RIRs")
    val unicastAssignmentV6 = toRecords(fetchIpv6(ianaOrgFileURL(IPV6_UNICAST_ASSIGNMENT)))
      .substract(ianaSlash23)
      .append(ianaSlash23)
      .append(toRecords(ipv6SpecialRegistry))
      .sorted()

    val reallocatedSpecial = toRecords(ipv4Reallocated ++ ipv4SpecialRegistry)
    reallocatedSpecial.append(unicastAssignmentV6)
  }

}
