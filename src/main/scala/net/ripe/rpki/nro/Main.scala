package net.ripe.rpki.nro

import scala.collection.parallel.immutable.ParMap

object Main extends App {

  val recordMaps: ParMap[String, Records] = Ports.fetchAndParse()

  // Adjusting and fixing record fields conforming to what is done by jeff.
  val rirs = (recordMaps - "iana" - "jeff").mapValues(_.fixRIRs)
  //val jeff = recordMaps("jeff")

  println(s"\n\n---  Combining RIRs data and checking for conflicts among RIRs ---\n\n")

  // Note we are not checking conflicts with IANA only among RIRs
  val (asns, asnConflicts)   = Merger.mergeAndDetectConflicts(rirs.values.map(_.asn))
  val (ipv4s, ipv4Conflicts) = Merger.mergeAndDetectConflicts(rirs.values.map(_.ipv4))
  val (ipv6s, ipv6Conflicts) = Merger.mergeAndDetectConflicts(rirs.values.map(_.ipv6))

  val iana = recordMaps("iana").fixIana
  val (asnIetf, ipv4Ietf, ipv6Ietf) = Iana.filterNonRIRs(iana)
  val (asnPool, ipv4pool, ipv6pool) = Iana.ianaPools(
    asns.keys  ++ asnIetf.keys,
    ipv4s.keys ++ ipv4Ietf.keys,
    ipv6s.keys ++ ipv6Ietf.keys
  )

  val asnCombined  = asns  ++ asnPool  ++ asnIetf
  val ipv4Combined = ipv4s ++ ipv4pool ++ ipv4Ietf
  val ipv6Combined = ipv6s ++ ipv6pool ++ ipv6Ietf

  Ports.writeCombined(asnCombined, ipv4Combined, ipv6Combined)
  Ports.writeConflicts(asnConflicts ++ ipv4Conflicts ++ ipv6Conflicts)

}
