package net.ripe.rpki.nro


object Main extends App {

  val (rirs, iana) = Ports.fetchAndParse()

  println(s"\n\n---  Combining RIRs data and checking for conflicts among RIRs ---\n\n")

  // Note we are not checking conflicts with IANA only among RIRs
  val (asns,  asnConflicts)  = Records.combineResources(rirs.map(_.asn))
  val (ipv4s, ipv4Conflicts) = Records.combineResources(rirs.map(_.ipv4))
  val (ipv6s, ipv6Conflicts) = Records.combineResources(rirs.map(_.ipv6))

  // Non RIRs are actually IETF allocated/assigned from iana.
  val (asnIetf, ipv4Ietf, ipv6Ietf) = Iana.filterNonRIRs(iana)

  // Ranges not used by RIRs and not IETF's become ianapools.
  val (asnPool, ipv4pool, ipv6pool) = Iana.ianaPools(
    asns.keys  ++ asnIetf.keys,
    ipv4s.keys ++ ipv4Ietf.keys,
    ipv6s.keys ++ ipv6Ietf.keys
  )

  val asnCombined  = Records.mergeSiblings(asns  ++ asnPool  ++ asnIetf)
  val ipv4Combined = Records.mergeSiblings(ipv4s ++ ipv4pool ++ ipv4Ietf)
  val ipv6Combined = Records.mergeSiblings(ipv6s ++ ipv6pool ++ ipv6Ietf)

  Ports.writeCombined(asnCombined, ipv4Combined, ipv6Combined)
  Ports.writeConflicts(asnConflicts ++ ipv4Conflicts ++ ipv6Conflicts)

}
