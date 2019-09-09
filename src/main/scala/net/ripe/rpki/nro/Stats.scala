package net.ripe.rpki.nro

import java.io._
import java.math.BigInteger

import net.ripe.commons.ip.Ipv6Range
import net.ripe.ipresource._

import scala.collection.JavaConverters._
import scala.collection.SortedMap
import scala.io.Source

import Defs._
import IO._ 

object Main extends App {

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
  def combine(resourceMap: Iterable[SortedMap[IpResourceRange, Record]]) =
    resourceMap.foldLeft(SortedMap[IpResourceRange, Record]())(merge)

  def combineAll() = {
    val (rirs, iana, jeff) = fetchAndParse

    // IETF reserved data from IANA is combined without modification
    val asnIetf  = iana.asn.filter { case (_, v)  => v.status == "ietf" || v.status == "assigned" }
    val ipv4Ietf = iana.ipv4.filter { case (_, v) => v.status == "ietf" || v.status == "assigned" }
    val ipv6Ietf = iana.ipv6.filter { case (_, v) => v.status == "ietf" || v.status == "assigned" }

    // Combine will also check for conflicts inter RIRS, not checking integrity within RIR
    println(s"\n\n---  Combining RIRs data and checking for conflicts ---\n\n")
    val asns  = combine(rirs.values.map(_.asn)) ++ asnIetf
    val ipv4s = combine(rirs.values.map(_.ipv4)) ++ ipv4Ietf
    val ipv6s = combine(rirs.values.map(_.ipv6)) ++ ipv6Ietf

    // Started with slash zero and remove one by one all ipv4s we found by combining RIRs, we ended up with ianapool
    val ianapool4 = new IpResourceSet
    ianapool4.add(IpResourceRange.parse("0.0.0.0/0"))
    ipv4s.keys.foreach(ianapool4.remove)

    // Ipv4 does not have to fit in bit boundary no need for prefix splitting.
    val ipv4sp = ipv4s ++ ianapool4.iterator.asScala
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> Ipv4Record.ianapool(a))
      .toMap

    // Started with slash zero and remove one by one all ipv6s we found by combining RIRs, we ended up with ianapool
    val ianapool6 = new IpResourceSet
    ianapool6.add(IpResourceRange.parse("::/0"))
    ipv6s.keys.foreach(ianapool6.remove)

    // IPv6 has to be fit into bit boundary, since the format is | start | prefixLength
    val ipv6sp = ipv6s ++ ianapool6.iterator.asScala
      .map(_.toString)
      .flatMap(s => Ipv6Range.parse(s).splitToPrefixes.asScala)
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> Ipv6Record.ianapool(a))
      .toMap

    val ianapoolAsn = new IpResourceSet
    ianapoolAsn.add(IpResourceRange.parse("AS0-AS4200000000"))
    asns.keys.foreach(ianapoolAsn.remove)
    val asnsp = asns ++ ianapoolAsn.iterator.asScala
      .map(a => IpResourceRange.parse(a.toString))
      .map(a => a -> AsnRecord.ianapool(a))
      .toMap

    writeOut(asnsp.values.toList, ipv4sp.values.toList, ipv6sp.values.toList)
    ((rirs, iana, jeff), (asnsp, ipv4sp, ipv6sp))

  }

  combineAll()
  println("Done")

}
