package net.ripe.rpki.nro

import net.ripe.ipresource.{IpResource, IpResourceSet}
import net.ripe.rpki.nro.Defs._

import scala.collection.JavaConverters._
import scala.collection.parallel.ParIterable
import scala.collection.{Iterable, Iterator, SortedMap, mutable}

object Merger {

  val conflicts : mutable.MutableList[(Record, Record)] = mutable.MutableList[(Record,Record)]()

  def checkAndMerge(currentResult: RecordsAndConflicts, nextRecords: SortedRecordsMap): RecordsAndConflicts = {
    val (currentMerge, currentConflict) = currentResult
    val mergedKeys = currentMerge.keySet
    val nextKeys   = nextRecords.keySet

    val newConflicts = mergedKeys.intersect(nextKeys)
      .map(k => Conflict(currentMerge(k), nextRecords(k)))


    (currentMerge ++ nextRecords, currentConflict ++ newConflicts)
  }

  // Combining multiple maps from different RIRs
  def combine(resourceMap: ParIterable[SortedRecordsMap]): (SortedRecordsMap, List[Conflict]) =
    resourceMap.foldLeft((SortedMap[IpResource, Record](), List[Conflict]()))(checkAndMerge)

  def subtractRanges(slash0: IpResource, used : Iterable[IpResource]): Iterator[IpResource] = {
    val allSpace = new IpResourceSet(slash0)
    used.foreach(allSpace.remove)
    allSpace.iterator.asScala
  }
}
