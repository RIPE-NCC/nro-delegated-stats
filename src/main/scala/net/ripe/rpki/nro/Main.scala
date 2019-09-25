package net.ripe.rpki.nro

import org.slf4j.LoggerFactory
import Settings._

object Main extends App {

  val logger = LoggerFactory.getLogger(Main.getClass)

  val (rirs, iana, previousConflicts) = Ports.fetchAndParse()

  val notifer = new Notifier(Settings.mailer)

  logger.info(s"\n\n---  Combining RIRs data and checking for conflicts among RIRs ---\n\n")

  // Non RIRs are actually IETF allocated/assigned from iana.
  logger.info("Calculating IETF")
  val (asnIetf , ipv4Ietf, ipv6Ietf) = Iana.filterNonRIRs(iana)

  // Note we are not checking conflicts with IANA only among RIRs
  logger.info("Combine and detect conflict  ASNs")
  val (asns,  asnConflicts)  = Merger.combineResources(rirs.map(_.asn)  ++ Iterable(asnIetf) )
  logger.info("Combine and detect conflict  IPv4")
  val (ipv4s, ipv4Conflicts) = Merger.combineResources(rirs.map(_.ipv4) ++ Iterable(ipv4Ietf) )
  logger.info("Combine and detect conflict  IPv6")
  val (ipv6s, ipv6Conflicts) = Merger.combineResources(rirs.map(_.ipv6) ++ Iterable(ipv6Ietf)  )


  logger.info("Calculating IANAPOOL")
  // Ranges not used by RIRs and not IETF's become ianapools.
  val (asnPool, ipv4pool, ipv6pool) = Iana.ianaPools( asns.map(_.range), ipv4s.map(_.range), ipv6s.map(_.range))

  val asnCombined  = (asns  ++ asnPool).sorted
  val ipv4Combined = (ipv4s ++ ipv4pool).sorted
  val ipv6Combined = (ipv6s ++ ipv6pool).sorted


  logger.info("Merging ASNs siblings ")
  val asnMerged  = Merger.mergeSiblings(asnCombined)
  logger.info("Merging IPv4 siblings ")
  val ipv4Merged = Merger.mergeSiblings(ipv4Combined)
  logger.info("Merging IPv6 siblings ")
  val ipv6Merged = Merger.mergeSiblings(ipv6Combined)

  val currentConflicts = asnConflicts ++ ipv4Conflicts ++ ipv6Conflicts

  Ports.writeResult(asnCombined, ipv4Combined, ipv6Combined, currentResultFile)
  Ports.writeResult(asnMerged, ipv4Merged, ipv6Merged, mergedFileName)
  Ports.writeConflicts(currentConflicts)


  notifer.notifyConflicts(currentConflicts, previousConflicts)

}
