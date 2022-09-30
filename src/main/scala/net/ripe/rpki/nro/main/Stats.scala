package net.ripe.rpki.nro.main

import net.ripe.rpki.nro.Const._
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.iana.IanaPools
import net.ripe.rpki.nro.model.{Conflict, Record, Records}
trait Stats extends Logging with Merger {

  // Main steps of stats merging and conflict detections.
  def process(rirRecords: Iterable[Records],
              ianaRecord: Records,
              previousResult: Option[Records]): (Records, Records, Seq[Conflict], Records, Records) = {

    logger.info(s"\n\n---  Combining RIRs data and checking for conflicts among RIRs ---\n\n")

    // Splitting iana into RIRs and those reserved for IETF or allocated to IANA
    val isRirRecord: Record => Boolean = r => RIRs.contains(r.stat.oid)
    val (ianaAllocatedRIRs, ianaReserved) = ianaRecord.partition(isRirRecord)

    val (combinedRIRsUsage, currentConflicts) = combineRecords(rirRecords ++ Iterable(ianaReserved), previousResult)

    logger.info("Calculate records unclaimed by RIRs while it is actually allocated by IANA")
    val ianaAllocatedUnclaimed: Records = ianaAllocatedRIRs.substract(combinedRIRsUsage).formatUnclaimed

    logger.info("Unclaimed ranges is resolved using data from previous delegated stats")
    val unclaimedResolved = previousResult
      .map { _.filter(_.status != "ianapool") }
      .map { resolveUnclaimed(ianaAllocatedUnclaimed, _) }
      .getOrElse(ianaAllocatedUnclaimed)

    logger.info("Calculate overclaimed, where RIRs are using more than what is allocated initially by Iana")
    val overclaimed: Records = combinedRIRsUsage.substract(ianaRecord)

    logger.info("Calculating IANA pool")
    val ianaPools: Records = IanaPools(combinedRIRsUsage.append(unclaimedResolved))

    val results: Records = combinedRIRsUsage.append(ianaPools).append(unclaimedResolved).sorted()

    logger.info("Merging ASNs siblings ")
    val mergedResults: Records = mergeRecords(results)

    (results, mergedResults, currentConflicts, unclaimedResolved, overclaimed)
  }

}
