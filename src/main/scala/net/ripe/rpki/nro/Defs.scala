package net.ripe.rpki.nro

import net.ripe.ipresource.IpResourceRange

object Defs {

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

  val IPV4_IANA_POOL_DATE = "20120801"

  val IPV4 = "ipv4"
  val IPV6 = "ipv6"
  val ASN = "asn"

  val ASSIGNED = "assigned"
  val ALLOCATED = "allocated"

  val RESERVED = "reserved"
  val AVAILABLE = "available"

  val JEFF = "jeff"

  val SUMMARY = "summary"

  val ALL_IPV4 = "0.0.0.0/0"
  val ALL_IPV6 = "::/0"
  val ALL_ASNS = "AS0-AS4200000000"

  type Line = Array[String]

  val TODAY: String = java.time.LocalDate.now.toString.replaceAll("-", "")

  implicit val ipResourceRangeOrder: Ordering[IpResourceRange] = (a: IpResourceRange, b: IpResourceRange) => a.compareTo(b)
}



