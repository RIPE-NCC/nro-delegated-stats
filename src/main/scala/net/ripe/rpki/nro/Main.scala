package net.ripe.rpki.nro


object Main extends App {

  val (rirs, iana) = Ports.fetchAndParse()

  println(s"\n\n---  Combining RIRs data and checking for conflicts among RIRs ---\n\n")

  // Non RIRs are actually IETF allocated/assigned from iana.
  println("Calculating IETF")
  val (asnIetf , ipv4Ietf, ipv6Ietf) = Iana.filterNonRIRs(iana)

  // Note we are not checking conflicts with IANA only among RIRs
  println("Combine and detect conflict  ASNs")
  val (asns,  asnConflicts)  = Records.combineResources(Iterable(asnIetf) ++ rirs.map(_.asn) )
  println("Combine and detect conflict  IPv4")
  val (ipv4s, ipv4Conflicts) = Records.combineResources(Iterable(ipv4Ietf) ++ rirs.map(_.ipv4) )
  println("Combine and detect conflict  IPv6")
  val (ipv6s, ipv6Conflicts) = Records.combineResources(Iterable(ipv6Ietf) ++ rirs.map(_.ipv6) )


  println("Calculating IANAPOOL")
  // Ranges not used by RIRs and not IETF's become ianapools.
  val (asnPool, ipv4pool, ipv6pool) = Iana.ianaPools( asns.map(_.range), ipv4s.map(_.range), ipv6s.map(_.range))

  // NOTE: What happened inside this merge siblling calls are no longer map operations !
  // IETF ++ no longer overrides, it used to be map operations, and now its just list append operations.
  // POOL are safely ++'ed since they are exclusive. Seems to me now IETF has to be merged earlier. 
  val asnCombined  = (asns  ++ asnPool).sorted
  val ipv4Combined = (ipv4s ++ ipv4pool).sorted
  val ipv6Combined = (ipv6s ++ ipv6pool).sorted


  println("Merging ASNs siblings ")
  val asnMerged  = Records.mergeSiblings(asnCombined)
  println("Merging IPv4 siblings ")
  val ipv4Merged = Records.mergeSiblings(ipv4Combined)
  println("Merging IPv6 siblings ")
  val ipv6Merged = Records.mergeSiblings(ipv6Combined)

  Ports.writeResult(asnCombined, ipv4Combined, ipv6Combined, "result/combined-stat")
  Ports.writeResult(asnMerged, ipv4Merged, ipv6Merged, "result/merged-stat")
  Ports.writeConflicts(asnConflicts ++ ipv4Conflicts ++ ipv6Conflicts)
}
