package net.ripe.rpki.nro.main

import java.math.BigInteger

import com.google.common.collect.{Range, RangeMap, TreeRangeMap}
import net.ripe.rpki.nro.Logging
import net.ripe.rpki.nro.model.{Conflict, Record, Records}
import scala.jdk.CollectionConverters._
import net.ripe.rpki.nro.Const._

trait Merger extends Logging with Ranges {

  def resolveConflict(newRange: Range[BigInteger], newRecord: Record,
                      currentMap: RangeMap[BigInteger, Record],
                      previousMap: RangeMap[BigInteger, Record]
                     ): Seq[Conflict] = {

    val currentOverlaps = currentMap.subRangeMap(newRange).asMapOfRanges().asScala

    // No overlaps between currentMap and newRange
    if (currentOverlaps.isEmpty) {
      currentMap.put(newRange, newRecord)
      Seq[Conflict]()
    } else {

      // Recording new conflicts.
      val newConflicts = currentOverlaps.values.map(Conflict(_, newRecord)).toSeq

      // Iana always wins
      if(newRecord.stat.registry == IANA){
        currentMap.put(newRange, newRecord)
      } else {
        // Look up if new range is on previous Map
        val previousOverlaps = previousMap.subRangeMap(newRange)

        if (!previousOverlaps.asMapOfRanges().isEmpty) {
          // Use previous values to resolve conflict.
          currentMap.putAll(previousOverlaps)
        } else {
          // Can't resolve using previous map, use the new Range
          currentMap.put(newRange, newRecord)
        }
      }

      newConflicts
    }
  }

  /**
   * Combine resources and detect conflict for certain type of resources of a Records (asn, ipv4 or ipv6).
   *
   * @param rirRecords        RIR's resources that we want to combine  e.g  Iterable( arin's ASN, ripe's ASN, ... )
   * @param previousRecords   Delegated claims from previous day that will be used to resolve conflict.
   * @return                  (combinedResources : List[Record], conflicts : List[Conflict])
   *                          combinedResources will be non overlapping resources from all the RIRs
   *                          conflicts: pairs of record that are conflicting.
   */
  def combineResources(rirRecords: Iterable[Seq[Record]],
                       previousRecords: Seq[Record] = Seq()
                      ): (Seq[Record], Seq[Conflict]) = {

    val previousMap: RangeMap[BigInteger, Record] = asRangeMap(previousRecords)
    val currentMap = TreeRangeMap.create[BigInteger, Record]()

    // Detecting conflicts while adding new records. Latest one added prevails.
    val conflicts = rirRecords.flatten.toSeq.flatMap { newRecord =>
      resolveConflict(newRecord.range.key, newRecord, currentMap, previousMap)
    }

    (alignRecordWithMapRangeKeys(currentMap), conflicts.toSeq)
  }

  /**
   * Merge adjacent records, example from:
   * apnic|ZZ|ipv4|0.0.0.0|256|20110412|assigned|A92319D5|e-stats
   * apnic|ZZ|ipv4|1.1.9.0|256|20110412|assigned|A92319D5|e-stats
   * apnic|ZZ|ipv4|1.1.10.0|512|20110412|assigned|A92319D5|e-stats
   * apnic|ZZ|ipv4|1.1.12.0|1024|20110412|assigned|A92319D5|e-stats
   * apnic|ZZ|ipv4|1.1.16.0|4096|20110412|assigned|A92319D5|e-stats
   * apnic|ZZ|ipv4|1.1.32.0|8192|20110412|assigned|A92319D5|e-stats
   *
   * Becomes:
   * apnic|ZZ|ipv4|0.0.0.0|256|20110412|assigned|A92319D5|e-stats
   * apnic|ZZ|ipv4|1.1.9.0|14080|20110412|assigned|A92319D5|e-stats
   *
   * @param records
   * @return
   */

  def mergeSiblings(records: Seq[Record]): Seq[Record] = {

    val sortedRecords = records.sorted

    var result: Seq[Record] = Nil
    var lastRecord = sortedRecords.head

    sortedRecords.tail foreach { nextRecord =>
      if (lastRecord.canMerge(nextRecord)) {
        lastRecord = lastRecord.merge(nextRecord)
        // look back and eat mergeable records
        while (result.nonEmpty && result.head.canMerge(lastRecord)) {
          lastRecord = result.head.merge(lastRecord)
          result = result.tail
        }
      }
      else {
        // keep non mergeable next records
        result = lastRecord +: result
        lastRecord = nextRecord
      }
    }
    // list append head operations, need to be reversed
    (lastRecord +: result).reverse
  }

  /**
   * Combine and detect conflict for each resource type. See combineResources for detail.
   * @param currentRecords
   * @param previous
   * @return
   */
  def combineRecords(currentRecords: Iterable[Records], previous: Option[Records] = None): (Records, Seq[Conflict]) = {

    logger.info("Combine and detect conflict Asn")
    val (asns, asnConflicts)   = combineResources(currentRecords.map(_.asn), previous.map(_.asn).getOrElse(Seq()))

    logger.info("Combine and detect conflict IPv4")
    val (ipv4s, ipv4Conflicts) = combineResources(currentRecords.map(_.ipv4), previous.map(_.ipv4).getOrElse(Seq()))

    logger.info("Combine and detect conflict IPv6")
    val (ipv6s, ipv6Conflicts) = combineResources(currentRecords.map(_.ipv6), previous.map(_.ipv6).getOrElse(Seq()))

    (Records(asns, ipv4s, ipv6s), asnConflicts ++ ipv4Conflicts ++ ipv6Conflicts)
  }

  /**
   * Merge adjacent records, see mergeSibling for detail.
   * @param records
   * @return
   */
  def mergeRecords(records: Records): Records = {
    logger.info("Merging ASNs siblings ")
    val asnMerged = mergeSiblings(records.asn)
    logger.info("Merging IPv4 siblings ")
    val ipv4Merged = mergeSiblings(records.ipv4)
    logger.info("Merging IPv6 siblings ")
    val ipv6Merged = mergeSiblings(records.ipv6)

    Records(asnMerged, ipv4Merged, ipv6Merged)
  }

  /**
   * Resolving the unclaimed ranges. Unclaimed => Allocated by IANA for certain RIR, but not claimed in their delegated stats.
   * Here we wanted to find if any RIR claimed it the day before. The resolution of the unclaimed should refer to previous day.
   * @param unclaimed
   * @param previous
   * @return
   */
  def resolveUnclaimed(unclaimed:Records, previous: Records) = {
    def resolveUnclaimedResource(prevRecords: Seq[Record], currRecords: Seq[Record]) = {
      val previousMap: RangeMap[BigInteger, Record] = asRangeMap(prevRecords)
      val currentMap = TreeRangeMap.create[BigInteger, Record]()

      currRecords.map { record =>
        val range: Range[BigInteger] = record.range.key
        val previousOverlaps = previousMap.subRangeMap(range)

        if (!previousOverlaps.asMapOfRanges().isEmpty)
          currentMap.putAll(previousOverlaps)
        else {
          currentMap.put(range, record)
        }

      }
      alignRecordWithMapRangeKeys(currentMap)
    }

    Records(
      resolveUnclaimedResource(previous.asn, unclaimed.asn),
      resolveUnclaimedResource(previous.ipv4, unclaimed.ipv4),
      resolveUnclaimedResource(previous.ipv6, unclaimed.ipv6))
  }

}
