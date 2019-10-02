package net.ripe.rpki.nro

import java.math.BigInteger

import com.google.common.collect.{Range, RangeMap, TreeRangeMap}
import net.ripe.rpki.nro.Ranges._

import scala.jdk.CollectionConverters._

object Merger extends Logging {

  def resolveConflict(newRange: Range[BigInteger], newRecord: Record,
                      currentMap: RangeMap[BigInteger, Record],
                      previousMap: RangeMap[BigInteger, Record]
                     ): List[Conflict] = {

    val currentOverlaps = currentMap.subRangeMap(newRange).asMapOfRanges().asScala
    if (currentOverlaps.isEmpty) {
      currentMap.put(newRange, newRecord)
      List[Conflict]()
    } else {

      val newConflicts = currentOverlaps.values.map(Conflict(_, newRecord)).toList
      val previousOverlaps = previousMap.subRangeMap(newRange)

      if (!previousOverlaps.asMapOfRanges().isEmpty) {
        currentMap.putAll(previousOverlaps)
      } else {
        currentMap.put(newRange, newRecord)
      }
      newConflicts
    }
  }

  def combineResources(rirRecords: Iterable[List[Record]],
                       previousRecords: List[Record] = List()
                      ): (List[Record], List[Conflict]) = {

    val previousMap = asRangeMap(previousRecords)
    val currentMap = TreeRangeMap.create[BigInteger, Record]()

    // Detecting conflicts while adding new records. Latest one added prevails.
    val conflicts = rirRecords.flatten.toList.flatMap { newRecord =>
      resolveConflict(newRecord.range.key, newRecord, currentMap, previousMap)
    }

    (updateMap(currentMap), conflicts)
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
    val (asns, asnConflicts)   = combineResources(currentRecords.map(_.asn))

    logger.info("Combine and detect conflict IPv4")
    val (ipv4s, ipv4Conflicts) = combineResources(currentRecords.map(_.ipv4))

    logger.info("Combine and detect conflict IPv6")
    val (ipv6s, ipv6Conflicts) = combineResources(currentRecords.map(_.ipv6))

    (Records(asns, ipv4s, ipv6s), asnConflicts ++ ipv4Conflicts ++ ipv6Conflicts)
  }

  def mergeRecords(records: Records): Records = {
    logger.info("Merging ASNs siblings ")
    val asnMerged = Merger.mergeSiblings(records.asn)
    logger.info("Merging IPv4 siblings ")
    val ipv4Merged = Merger.mergeSiblings(records.ipv4)
    logger.info("Merging IPv6 siblings ")
    val ipv6Merged = Merger.mergeSiblings(records.ipv6)

    Records(asnMerged, ipv4Merged, ipv6Merged)
  }
}

