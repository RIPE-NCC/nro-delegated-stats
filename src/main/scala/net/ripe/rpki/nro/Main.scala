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
    val (asns, asnConflicts)   = combine(rirs.values.map(_.asn))
    val (ipv4s, ipv4Conflicts) = combine(rirs.values.map(_.ipv4))
    val (ipv6s, ipv6Conflicts) = combine(rirs.values.map(_.ipv6))

    val ipv4pool = subtractRanges(ALL_IPV4, ipv4s.keys ++ ipv4Ietf.keys).map(ipv4 => ipv4 -> Ipv4Record.ianapool(ipv4)).toMap
    val asnPool  = subtractRanges(ALL_ASNS, asns.keys  ++ asnIetf.keys) .map(asn  => asn  -> AsnRecord.ianapool(asn)).toMap

    // Ipv6 needs to adjusted/split to bit boundary using other library (commons ip math)
    // To String and parse are converting between these ip libraries
    val ipv6pool = subtractRanges(ALL_IPV6, ipv6s.keys ++ ipv6Ietf.keys).map(_.toString)
      .flatMap(s => Ipv6Range.parse(s).splitToPrefixes.asScala)
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> Ipv6Record.ianapool(a))
      .toMap

    val List(asnsWithPool, ipv4sWithPool, ipv6sWithPool) = 
      List(asns ++ asnPool ++ asnIetf, ipv4s ++ ipv4pool ++ ipv4Ietf, ipv6s ++ ipv6pool ++ ipv6Ietf).map(_.values.toList)

    // Dump to file output, see on results dir.
    writeOut(asnsWithPool, ipv4sWithPool, ipv6sWithPool)


    writeConflicts(asnConflicts++ipv4Conflicts++ipv6Conflicts)

}
