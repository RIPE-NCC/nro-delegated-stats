package net.ripe.rpki.nro

import net.ripe.commons.ip._
import net.ripe.ipresource._
import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Ports._

import scala.collection.JavaConverters._
import scala.collection._
import scala.collection.parallel.ParIterable
import scala.collection.parallel.immutable.ParMap

object Stats extends App {

  // I guess we can do conflict detection here.
  def merge(ma: SortedMap[IpResourceRange, Record], mb: SortedMap[IpResourceRange, Record]) = {
    val ka       = ma.keySet
    val kb       = mb.keySet
    val conflict = ka.intersect(kb)
    conflict.foreach(k => {
      val va = ma(k)
      val vb = mb(k)

      println(s"$k is found both on ${va.registry} and ${vb.registry}")
      println(s"<    $va ")
      println(s">    $vb ")
    })
    ma ++ mb
  }

  // Combining multiple maps from different RIRs
  def combine(resourceMap: ParIterable[SortedMap[IpResourceRange, Record]]): SortedMap[IpResourceRange, Record] =
    resourceMap.foldLeft(SortedMap[IpResourceRange, Record]())(merge)

  def createDelegatedStats(): Unit = {

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

    // Started with slash zero 
    val allIpv4 = new IpResourceSet
    allIpv4.add(IpResourceRange.parse("0.0.0.0/0"))
    // Remove all ipv4s we found from RIRs for ianapool
    ipv4s.keys.foreach(allIpv4.remove)

    // We process the remaining range into iana pool 
    val ipv4pool =  allIpv4.iterator.asScala
      .map(a => IpResourceRange.range(a.getStart, a.getEnd))
      .map(a => a -> Ipv4Record.ianapool(a))
      .toMap

    // Started with slash zero 
    val allIpv6 = new IpResourceSet
    allIpv6.add(IpResourceRange.parse("::/0"))

    // Remove all ipv6s we found from RIRs for ianapool
    ipv6s.keys.foreach(allIpv6.remove)

    // IPv6 has to be fit into bit boundary, since the format is | start | prefixLength
    val ipv6pool = allIpv6.iterator.asScala
      .map(_.toString)
      .flatMap(s => Ipv6Range.parse(s).splitToPrefixes.asScala)
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> Ipv6Record.ianapool(a))
      .toMap

    // Started with whole ASN range 
    val allAsn = new IpResourceSet
    allAsn.add(IpResourceRange.parse("AS0-AS4200000000"))

    // Remove asns we found so far from RIR for ianapool
    asns.keys.foreach(allAsn.remove)

    val asnPool = allAsn.iterator.asScala
      .map(a => IpResourceRange.range(a.getStart, a.getEnd))
      .map(a => a -> AsnRecord.ianapool(a))
      .toMap

    val List(asnsWithPool, ipv4sWithPool, ipv6sWithPool) = 
      List(asns ++ asnPool, ipv4s ++ ipv4pool, ipv6s ++ ipv6pool).map(_.values.toList)

    // Dump to file output, see on results dir.
    writeOut(asnsWithPool, ipv4sWithPool, ipv6sWithPool)

  }

  createDelegatedStats()
  println("Done")

}
