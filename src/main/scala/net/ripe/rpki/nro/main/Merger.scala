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
                     ): List[Conflict] = {

    val currentOverlaps = currentMap.subRangeMap(newRange).asMapOfRanges().asScala

    // No overlaps between currentMap and newRange
    if (currentOverlaps.isEmpty) {
      currentMap.put(newRange, newRecord)
      List[Conflict]()
    } else {

      // Recording new conflicts.
      val newConflicts = currentOverlaps.values.map(Conflict(_, newRecord)).toList

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

  def combineResources(rirRecords: Iterable[List[Record]],
                       previousRecords: List[Record] = List()
                      ): (List[Record], List[Conflict]) = {

    val previousMap: RangeMap[BigInteger, Record] = asRangeMap(previousRecords)
    val currentMap = TreeRangeMap.create[BigInteger, Record]()

    // Detecting conflicts while adding new records. Latest one added prevails.
    val conflicts = rirRecords.flatten.toList.flatMap { newRecord =>
      resolveConflict(newRecord.range.key, newRecord, currentMap, previousMap)
    }

    (alignRecordWithMapRangeKeys(currentMap), conflicts)
  }

  def mergeSiblings(records: List[Record]): List[Record] = {

    val sortedRecords = records.sorted

    var result: List[Record] = Nil
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
        result = lastRecord :: result
        lastRecord = nextRecord
      }
    }
    // list append head operations, need to be reversed
    (lastRecord :: result).reverse
  }

  def combineRecords(currentRecords: Iterable[Records], previous: Option[Records] = None): (Records, List[Conflict]) = {

    logger.info("Combine and detect conflict Asn")
    val (asns, asnConflicts)   = combineResources(currentRecords.map(_.asn), previous.map(_.asn).getOrElse(List()))

    logger.info("Combine and detect conflict IPv4")
    val (ipv4s, ipv4Conflicts) = combineResources(currentRecords.map(_.ipv4), previous.map(_.ipv4).getOrElse(List()))

    logger.info("Combine and detect conflict IPv6")
    val (ipv6s, ipv6Conflicts) = combineResources(currentRecords.map(_.ipv6), previous.map(_.ipv6).getOrElse(List()))

    (Records(asns, ipv4s, ipv6s), asnConflicts ++ ipv4Conflicts ++ ipv6Conflicts)
  }

  def mergeRecords(records: Records): Records = {
    logger.info("Merging ASNs siblings ")
    val asnMerged = mergeSiblings(records.asn)
    logger.info("Merging IPv4 siblings ")
    val ipv4Merged = mergeSiblings(records.ipv4)
    logger.info("Merging IPv6 siblings ")
    val ipv6Merged = mergeSiblings(records.ipv6)

    Records(asnMerged, ipv4Merged, ipv6Merged)
  }

  def resolveUnclaimed(unclaimed:Records, previous: Option[Records]) = {
      // Let all unclaimed conflicted with itself and resolved to previous, discard conflict.
      val (result, _) = combineRecords(Iterable(unclaimed, unclaimed), previous)
      result 
  }
    
}
