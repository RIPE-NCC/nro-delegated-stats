package net.ripe.rpki.nro

import net.ripe.commons.ip.{AsnRange, Ipv4Range, Ipv6Range}

object Const {

  val IANA = "iana"
  val IETF = "ietf"
  val IANAPOOL = "ianapool"

  val ARIN = "arin"
  val APNIC = "apnic"
  val LACNIC = "lacnic"
  val AFRINIC = "afrinic"
  val RIPENCC = "ripencc"

  val RSCG = "rscg"

  val DEFAULT_CC = "ZZ"
  val DEFAULT_EXT = "e-stats"


  // IANA Source files

  val ASN16 = "asn16"
  val ASN32 = "asn32"

  val IPV4_ADDRESS_SPACE = "ipv4-address-space"
  val IPV6_ADDRESS_SPACE = "ipv6-address-space"

  val IPV4_RECOVERED_SPACE   = "ipv4-recovered-space"
  val IPV4_REALLOCATED_SPACE = "ipv4-reallocated-space"

  val IPV4_SPECIAL_REGISTRY  = "ipv4-special-registry"
  val IPV6_SPECIAL_REGISTRY  = "ipv6-special-registry"
  val ASN_SPECIAL_REGISTRY   = "asn-special-registry"

  val IPV6_UNICAST_ASSIGNMENT  = "ipv6-unicast-assignment"

// Available ranges that is not listed on IANA files are dated with these fillers.
  val IPV4_IANA_POOL_DATE = "20120801"
  val IPV6_IANA_POOL_DATE = "19960801"
  val ASN_IANA_POOL_DATE = "20061129"

  val ASN_UNALLOCATED_DATE = "20061129"
  val ASN_EMPTY_DATE = "19921201"
  val ASN_SPECIAL_DATE = "20140311"

  val IPV6_RFC_DATE = "19960801"

  val IPV4 = "ipv4"
  val IPV6 = "ipv6"
  val ASN = "asn"

  val ASSIGNED = "assigned"
  val ALLOCATED = "allocated"

  val RESERVED = "reserved"
  val AVAILABLE = "available"

  val SUMMARY = "summary"

  val ALL_ASNS: AsnRange  = AsnRange.parse("AS0-AS4200000000")
  val ALL_IPV6: Ipv6Range = Ipv6Range.parse("::/0")
  val ALL_IPV4: Ipv4Range = Ipv4Range.parse("0.0.0.0/0")

  // Magic serial number, all previous delegated-extended-stat are using this serial number
  val MAGIC_SERIAL_NUMBER = 19821213

  val RIRs: List[String] = List(APNIC, AFRINIC, ARIN, LACNIC, RIPENCC)
}



