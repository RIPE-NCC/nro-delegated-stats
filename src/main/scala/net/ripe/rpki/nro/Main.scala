package net.ripe.rpki.nro

object Main extends App {

  val (rirs, iana) = Ports.fetchAndParse()

  println(s"\n\n---  Combining RIRs data and checking for conflicts among RIRs ---\n\n")

  // Non RIRs are actually IETF allocated/assigned from iana.
  println("Calculating IETF")
  val (asnIetf , ipv4Ietf, ipv6Ietf) = Iana.filterNonRIRs(iana)

  // Note we are not checking conflicts with IANA only among RIRs
  println("Combine and detect conflict  ASNs")
  val (asns,  asnConflicts)  = Merger.combineResources(rirs.map(_.asn)  ++ Iterable(asnIetf) )
  println("Combine and detect conflict  IPv4")
  val (ipv4s, ipv4Conflicts) = Merger.combineResources(rirs.map(_.ipv4) ++ Iterable(ipv4Ietf) )
  println("Combine and detect conflict  IPv6")
  val (ipv6s, ipv6Conflicts) = Merger.combineResources(rirs.map(_.ipv6) ++ Iterable(ipv6Ietf)  )


  println("Calculating IANAPOOL")
  // Ranges not used by RIRs and not IETF's become ianapools.
  val (asnPool, ipv4pool, ipv6pool) = Iana.ianaPools( asns.map(_.range), ipv4s.map(_.range), ipv6s.map(_.range))

  val asnCombined  = (asns  ++ asnPool).sorted
  val ipv4Combined = (ipv4s ++ ipv4pool).sorted
  val ipv6Combined = (ipv6s ++ ipv6pool).sorted


  println("Merging ASNs siblings ")
  val asnMerged  = Merger.mergeSiblings(asnCombined)
  println("Merging IPv4 siblings ")
  val ipv4Merged = Merger.mergeSiblings(ipv4Combined)
  println("Merging IPv6 siblings ")
  val ipv6Merged = Merger.mergeSiblings(ipv6Combined)

  Ports.writeResult(asnCombined, ipv4Combined, ipv6Combined, "result/combined-stat")
  Ports.writeResult(asnMerged, ipv4Merged, ipv6Merged, "result/merged-stat")
  Ports.writeConflicts(asnConflicts ++ ipv4Conflicts ++ ipv6Conflicts)
}
