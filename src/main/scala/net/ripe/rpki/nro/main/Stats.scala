package net.ripe.rpki.nro.main

import net.ripe.rpki.nro.Const.RIRS
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.model.{Conflict, Record, Records}

trait Stats extends Logging with Merger {

  // Main steps of stats merging and conflict detections.
  def process(rirRecords: Iterable[Records], ianaRecord: Records, previousConflicts: List[Conflict]): (Records, Records, List[Conflict], Records, Records) = {

    logger.info(s"\n\n---  Combining RIRs data and checking for conflicts among RIRs ---\n\n")

    // Splitting iana into RIRs and non RIRs (IETF, IANA)
    val isRirRecord: Record => Boolean = r => RIRS.contains(r.status)
    val (ianaRirs, ianaNonRirs) = ianaRecord.partition(isRirRecord)

    val (combined, currentConflicts) = combineRecords(rirRecords ++ Iterable(ianaNonRirs))

    logger.info("Calculate unclaimed")
    val unclaimed: Records = ianaRirs.substract(combined).fixUnclaimed

    logger.info("Calculate overclaimed")
    val overclaimed = combined.substract(ianaRecord)

    logger.info("Calculating IANA pool")
    val ianaPools = IanaPools(combined.append(unclaimed))

    val results = combined.append(ianaPools).append(unclaimed).sorted()

    logger.info("Merging ASNs siblings ")
    val mergedResults = mergeRecords(results)

    (results, mergedResults, currentConflicts, unclaimed, overclaimed)
  }

}
