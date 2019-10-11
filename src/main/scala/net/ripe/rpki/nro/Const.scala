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

  val DEFAULT_CC = "ZZ"
  val DEFAULT_EXT = "e-stats"

  // Ipv4 ianapool on Geoff's combined results always dated 20120801, Magic date, where is it from?
  val IPV4_IANA_POOL_DATE = "20120801"

  val IPV4 = "ipv4"
  val IPV6 = "ipv6"
  val ASN = "asn"

  val ASSIGNED = "assigned"
  val ALLOCATED = "allocated"

  val RESERVED = "reserved"
  val AVAILABLE = "available"

  val GEOFF = "geoff"

  val SUMMARY = "summary"

  val ALL_ASNS: AsnRange  = AsnRange.parse("AS0-AS4200000000")
  val ALL_IPV6: Ipv6Range = Ipv6Range.parse("::/0")
  val ALL_IPV4: Ipv4Range = Ipv4Range.parse("0.0.0.0/0")

  // Magic serial number, all previous delegated-extended-stat are using this serial number
  val MAGIC_SERIAL_NUMBER = 19821213

  val RIRS = List(APNIC, AFRINIC, ARIN, LACNIC, RIPENCC)
}



