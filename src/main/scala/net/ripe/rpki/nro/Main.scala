package net.ripe.rpki.nro

import Defs._
import Settings._
import org.slf4j.{Logger, LoggerFactory}

object Main extends App {

  val logger: Logger = LoggerFactory.getLogger(Main.getClass)

  val (rirRecords, ianaRecord, previousConflicts) = Ports.fetchAndParse()

  logger.info(s"\n\n---  Combining RIRs data and checking for conflicts among RIRs ---\n\n")

  // Splitting iana into RIRs and non RIRs (IETF, IANA)
  val isRirRecord: Record => Boolean = r => RIRS.contains(r.status)
  val (ianaRirs, ianaNonRirs) = ianaRecord.partition(isRirRecord)

  val (combined, currentConflicts) = Merger.combineRecords(rirRecords ++ Iterable(ianaNonRirs))

  logger.info("Calculate unclaimed")
  val unclaimed = ianaRirs.substract(combined)

  logger.info("Calculate overclaimed")
  val overclaimed = combined.substract(ianaRecord)

  logger.info("Calculating IANA pool")
  val ianaPools = Iana.ianaPools(combined)

  val results = combined.append(ianaPools).sorted()

  logger.info("Merging ASNs siblings ")
  val mergedResults = Merger.mergeRecords(results)

  val notifier = new Notifier(Settings.mailer)
  notifier.notifyConflicts(currentConflicts, previousConflicts)

  Ports.writeRecords(results, currentResultFile)
  Ports.writeRecords(mergedResults, currentMergedFile)
  Ports.writeConflicts(currentConflicts, currentConflictFile)

  Ports.writeClaims(unclaimed, currentUnclaimedFile)
  Ports.writeClaims(overclaimed, currentOverclaimedFile)

}
