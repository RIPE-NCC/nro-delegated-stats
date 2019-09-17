package net.ripe.rpki.nro

import java.math.BigInteger

import com.google.common.collect.{Range, RangeMap, TreeRangeMap}
import net.ripe.commons.ip.Ipv6Range
import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Updates._

import scala.collection.JavaConverters._
// Records holder for three type of records, contains some logic of fixing entries
case class Records(
                    source: String,
                    header: Line,
                    summaries: List[Line],
                    asn: List[AsnRecord],
                    ipv4: List[Ipv4Record],
                    ipv6: List[Ipv6Record]
                  ) {

  def fixIana: Records = {
    def fix[A <: Record : Updates]: A => A = (rec: A) => rec.status match {
      case IETF => rec.oid_(IETF).ext_(IANA)
      case IANA => rec.oid_(IANA).status_(ASSIGNED).ext_(IANA)
      case _ => rec.ext_(IANA)
    }

    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }

  // Reserved and available records normally have neither country code or date, when combined it will be filled with ZZ and today's date.
  def fixRIRs: Records = {
    def fix[A <: Record : Updates]: A => A = (rec: A) => rec.status match {
      // RESERVED and AVAILABLE has neither date nor country (except AFRINIC with ZZ)
      case RESERVED | AVAILABLE => rec.date_(TODAY).cc_(DEFAULT_CC)
      // Whatever allocated in original RIR file appears as ASSIGNED
      case ALLOCATED => rec.status_(ASSIGNED)
      case _ => rec
    }

    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }

  override def toString: String =
    s"""Source: $source \nHeader: ${header.mkString(",")} \nSummaries: \n${
      summaries
        .map(_.mkString(","))
        .mkString("\n")
    }\n\n"""

  // Map values wrapper
  def asn_(f: AsnRecord => AsnRecord)(implicit ev: Updates[AsnRecord]): Records =
    this.copy(asn = this.asn.map(f))

  def ipv4_(f: Ipv4Record => Ipv4Record)(implicit ev: Updates[Ipv4Record]): Records =
    this.copy(ipv4 = this.ipv4.map(f))

  def ipv6_(f: Ipv6Record => Ipv6Record)(implicit ev: Updates[Ipv6Record]): Records =
    this.copy(ipv6 = this.ipv6.map(f))

}

object Records {

  def resolveConflict(newRange: Range[BigInteger], newRecord: Record,
                      currentMap: RangeMap[BigInteger, Record],
                      previousMap: RangeMap[BigInteger, Record]
                     ): List[Conflict] = {

      val currentOverlaps = currentMap.subRangeMap(newRange).asMapOfRanges()
      if(currentOverlaps.isEmpty) {
          currentMap.put(newRange, newRecord)
          List[Conflict]()
      } else {
        val newConflicts = currentOverlaps.asScala.map {
          case (_, conflict) => Conflict(conflict, newRecord)
        }
        val previousOverlaps = previousMap.subRangeMap(newRange)
        if(!previousOverlaps.asMapOfRanges().isEmpty) {
          val merged: RangeMap[BigInteger, Record] = TreeRangeMap.create[BigInteger,Record]()
          merged.put(newRange, newRecord)
          merged.putAll(previousOverlaps)
          currentMap.putAll(merged)
        } else
          currentMap.put(newRange, newRecord)
        newConflicts.toList
      }
  }

  def combineResources(rirRecords: Iterable[List[Record]],
                       previousRecords: List[Record] = List()
                      ) : (List[Record], List[Conflict]) = {

    val previousMap: RangeMap[BigInteger, Record] = TreeRangeMap.create[BigInteger,Record]()
    previousRecords.foreach(record => previousMap.put(recordRange(record) , record))

    val currentMap = TreeRangeMap.create[BigInteger, Record]()


    // Detecting conflicts while adding new records. Latest one added prevails.
    val conflicts = rirRecords.flatten.toList.flatMap { newRecord =>

      val newRange = recordRange(newRecord)

      resolveConflict(newRange, newRecord, currentMap, previousMap)
    }

    // Records needs to be updated, because as result of put into range maps above,
    // some new splitted ranges might be introduced. See RecordsTests
    val updatedRecords = currentMap.asMapOfRanges().asScala.toList.flatMap {
      case (range, record) => if(record.lType == "ipv6")
       {
        val (start, end) = Record.startEnd(range)
        // Guava RangeMap[BigInteger, Record] does not care about bit boundary
        // Special treatment needed to align IPv6 while updating.
        Ipv6Range.from(start).to(end).splitToPrefixes().asScala.map { ipv6 =>
          record.update(Range.closed(ipv6.start().asBigInteger(), ipv6.end().asBigInteger()))
        }
      } else {
        List(record.update(range))
      }
    }

    (updatedRecords, conflicts)
  }

  private def recordRange(newRecord: Record) = {
    Range.closed(newRecord.range.getStart.getValue, newRecord.range.getEnd.getValue)
  }

  def mergeSiblings(records: List[Record]): List[Record] = {

    val sortedRecords = records.sorted

    var result: List[Record] = Nil
    var lastRecord = sortedRecords.head

    sortedRecords.tail foreach { nextRecord =>
      if (lastRecord.canMerge(nextRecord)) {
        lastRecord = lastRecord.merge(nextRecord)
      }
      else {
        result = lastRecord :: result
        lastRecord = nextRecord
      }
    }

    (lastRecord :: result).reverse
  }

}
