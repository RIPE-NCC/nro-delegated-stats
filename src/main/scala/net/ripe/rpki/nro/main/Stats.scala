package net.ripe.rpki.nro.main

import net.ripe.rpki.nro.Const._
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.iana.IanaPools
import net.ripe.rpki.nro.model.{Conflict, Record, Records}
trait Stats extends Logging with Merger {

  // Main steps of stats merging and conflict detections.
  def process(rirRecords: Iterable[Records],
              ianaRecord: Records,
              previousResult: Option[Records]): (Records, Records, List[Conflict], Records, Records) = {

    logger.info(s"\n\n---  Combining RIRs data and checking for conflicts among RIRs ---\n\n")

    // Splitting iana into RIRs and non RIRs (IETF, IANA)
    val isRirRecord: Record => Boolean = r => RIRS.contains(r.stat.oid)
    val (ianaRirs, ianaNonRirs) = ianaRecord.partition(isRirRecord)

    val (combined, currentConflicts) = combineRecords(rirRecords ++ Iterable(ianaNonRirs), previousResult)

    logger.info("Calculate unclaimed")
    val unclaimed: Records = ianaRirs.substract(combined).formatUnclaimed

    logger.info("Fixing unclaimed with previous, non ianapool data")
    val unclaimedResolved = previousResult
      .map { _.filter(_.status != "ianapool") }
      .map { resolveUnclaimed(unclaimed, _) }
      .getOrElse(unclaimed)

    logger.info("Calculate overclaimed")
    val overclaimed: Records = combined.substract(ianaRecord)

    logger.info("Calculating IANA pool")
    val ianaPools: Records = IanaPools(combined.append(unclaimedResolved))

    val results: Records = combined.append(ianaPools).append(unclaimedResolved).sorted()

    logger.info("Merging ASNs siblings ")
    val mergedResults: Records = mergeRecords(results)

    (results, mergedResults, currentConflicts, unclaimedResolved, overclaimed)
  }

}
