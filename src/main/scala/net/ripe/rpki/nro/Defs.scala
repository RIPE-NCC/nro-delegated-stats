package net.ripe.rpki.nro

import net.ripe.ipresource.IpResource

import scala.collection.SortedMap

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

  val GEOFF = "geoff"

  val SUMMARY = "summary"

  val ALL_IPV4: IpResource = IpResource.parse("0.0.0.0/0")
  val ALL_IPV6: IpResource = IpResource.parse("::/0")
  val ALL_ASNS: IpResource = IpResource.parse("AS0-AS4200000000")

  val TODAY: String = java.time.LocalDate.now.toString.replaceAll("-", "")

  type Line = Array[String]
  type SortedRecordsMap = SortedMap[IpResource, Record]
  type RecordsAndConflicts = (SortedRecordsMap, List[Conflict])


  val dataSources: Map[String, String] = Map[String, String](
    APNIC   -> "http://ftp.apnic.net/stats/apnic/delegated-apnic-extended-latest",
    AFRINIC -> "http://ftp.afrinic.net/stats/afrinic/delegated-afrinic-extended-latest",
    ARIN    -> "http://ftp.arin.net/pub/stats/arin/delegated-arin-extended-latest",
    LACNIC  -> "http://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-extended-latest",
    RIPENCC -> "https://ftp.ripe.net/pub/stats/ripencc/delegated-ripencc-extended-latest",
    IANA    -> "http://ftp.apnic.net/pub/stats/iana/delegated-iana-latest",
    GEOFF   -> "https://www.nro.net/wp-content/uploads/apnic-uploads/delegated-extended"
  )

  val RIRS = List(APNIC, AFRINIC, ARIN, LACNIC, RIPENCC)
}



