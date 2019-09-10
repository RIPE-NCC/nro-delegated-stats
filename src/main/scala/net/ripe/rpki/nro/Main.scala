package net.ripe.rpki.nro

import net.ripe.commons.ip._
import net.ripe.ipresource._
import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Ports._

import scala.collection.JavaConverters._
import scala.collection.parallel.immutable.ParMap

import Merger._

object Main extends App {

    val recordMaps: ParMap[String, Records] = fetchAndParse()

    // Adjusting and fixing record fields conforming to what is done by jeff.
    val rirs = (recordMaps - "iana" - "jeff").mapValues(_.fixRIRs)
    val iana = recordMaps("iana").fixIana
    //val jeff = recordMaps("jeff")

    // Filter for iana data not allocated for RIRs
    val nonRirData: ((IpResourceRange, Record)) => Boolean =  {
      case (_,v) => !rirs.keySet.contains(v.status)
    }

    // Non RIRs a.k.a IETF reserved data from IANA is combined without modification
    val asnIetf  = iana.asn.filter(nonRirData)
    val ipv4Ietf = iana.ipv4.filter(nonRirData)
    val ipv6Ietf = iana.ipv6.filter(nonRirData)

    // Combine will also check for conflicts inter RIRS, not checking integrity within RIR
    println(s"\n\n---  Combining RIRs data and checking for conflicts within RIRs ---\n\n")
    // Note we are not checking conflicts with IANA only among RIRs
    val asns  = combine(rirs.values.map(_.asn)) ++ asnIetf
    val ipv4s = combine(rirs.values.map(_.ipv4)) ++ ipv4Ietf
    val ipv6s = combine(rirs.values.map(_.ipv6)) ++ ipv6Ietf

    val ipv4pool = substractSpace(ALL_IPV4, ipv4s.keys).map(ipv4 => ipv4 -> Ipv4Record.ianapool(ipv4)).toMap
    val asnPool  = substractSpace(ALL_ASNS, asns.keys) .map(asn  => asn  -> AsnRecord.ianapool(asn)).toMap

    // Ipv6 needs to adjusted/split to bit boundary using other library (commons ip math)
    // To String and parse are converting between these ip libraries
    val ipv6pool = substractSpace(ALL_IPV6, ipv6s.keys).map(_.toString)
      .flatMap(s => Ipv6Range.parse(s).splitToPrefixes.asScala)
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> Ipv6Record.ianapool(a))
      .toMap

    val List(asnsWithPool, ipv4sWithPool, ipv6sWithPool) = 
      List(asns ++ asnPool, ipv4s ++ ipv4pool, ipv6s ++ ipv6pool).map(_.values.toList)

    // Dump to file output, see on results dir.
    writeOut(asnsWithPool, ipv4sWithPool, ipv6sWithPool)

}
