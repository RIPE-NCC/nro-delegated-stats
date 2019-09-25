package net.ripe.rpki.nro

import java.math.BigInteger

import com.google.common.collect.{Range, RangeMap, TreeRangeMap}

import scala.jdk.CollectionConverters._

object Merger {

  def resolveConflict(newRange: Range[BigInteger], newRecord: Record,
                      currentMap: RangeMap[BigInteger, Record],
                      previousMap: RangeMap[BigInteger, Record]
                     ): List[Conflict] = {

    val currentOverlaps = currentMap.subRangeMap(newRange).asMapOfRanges().asScala
    if (currentOverlaps.isEmpty) {
      currentMap.put(newRange, newRecord)
      List[Conflict]()
    } else {

      val newConflicts = currentOverlaps.values.map(Conflict(_, newRecord))

      val previousOverlaps = previousMap.subRangeMap(newRange)
      if (!previousOverlaps.asMapOfRanges().isEmpty) {
        currentMap.putAll(previousOverlaps)
      } else {
        currentMap.put(newRange, newRecord)
      }
      newConflicts.toList
    }
  }

  def combineResources(rirRecords: Iterable[List[Record]],
                       previousRecords: List[Record] = List()
                      ): (List[Record], List[Conflict]) = {

    val previousMap: RangeMap[BigInteger, Record] = TreeRangeMap.create[BigInteger, Record]()
    previousRecords.foreach(record => previousMap.put(record.key, record))

    val currentMap = TreeRangeMap.create[BigInteger, Record]()

    // Detecting conflicts while adding new records. Latest one added prevails.
    val conflicts = rirRecords.flatten.toList.flatMap { newRecord =>
      resolveConflict(newRecord.key, newRecord, currentMap, previousMap)
    }

    // Records needs to be updated, because as result of put into range maps above,
    // some new splitted ranges might be introduced. See RecordsTests
    val updatedRecords = currentMap.asMapOfRanges().asScala.toList.flatMap {
      case (_, record) if record.empty => List()
      case (range, record: Ipv6Record) =>
        Ipv6Record.splitPrefixes(range).map { ipv6 =>
          record.update(Range.closed(ipv6.start().asBigInteger(), ipv6.end().asBigInteger()))
        }
      case (range, record) => List(record.update(range))
    }

    (updatedRecords, conflicts)
  }

  def mergeSiblings(records: List[Record]): List[Record] = {

    val sortedRecords = records.sorted

    var result: List[Record] = Nil
    var lastRecord = sortedRecords.head

    sortedRecords.tail foreach { nextRecord =>
      if (lastRecord.canMerge(nextRecord)) {
        lastRecord = lastRecord.merge(nextRecord)
        while(result.nonEmpty && result.head.canMerge(lastRecord)) {
          lastRecord = result.head.merge(lastRecord)
          result = result.tail
        }
      }
      else {
        result = lastRecord :: result
        lastRecord = nextRecord
      }

    }
    (lastRecord :: result).reverse
  }
}

