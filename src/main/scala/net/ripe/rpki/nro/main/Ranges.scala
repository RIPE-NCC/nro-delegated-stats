package net.ripe.rpki.nro.main

import java.math.BigInteger
import com.google.common.collect.{BoundType, DiscreteDomain, Range, RangeMap, TreeRangeMap}
import net.ripe.rpki.nro.model.{Ipv4Record, Ipv6Record, Record, RecordRange}

import scala.jdk.CollectionConverters._
/**
 * Range and RangeMap related utilities
 */
trait Ranges {

  def asRangeMap(recs: Seq[Record]): RangeMap[BigInteger, Record] = {
    val result: RangeMap[BigInteger, Record] = TreeRangeMap.create[BigInteger, Record]()
    recs.foreach(rec => result.put(rec.range.key, rec))
    result
  }

  /**
   *
   * This method aligns Records' ranges from a map  (Range[BigInteger] -> Record) where the keys (range) might no
   * longer reflect what is  in the Records. This could happen due to operations on the ranges (overlaps
   * resolutions, or subtractions of Records)
   *
   * Notice that for IPv6, we need to split the range into legal prefixes since potentially range operations above
   * produce illegal range. For ASN and IPv4 we need only to update the record's range.
   *
   * Empty case, is to avoid empty interval, a behaviour of RangeMap we used in Guava library. See RangeMapTest.
   *
   * @param mapToAlign  a map that will be aligned, where the key is a range that might no longer be aligned with
   *                    range in the record.
   * @return List[Record] where the ranges in these records are already aligned with the keys on the original map to
   *         Align
   */
  def alignRecordWithMapRangeKeys(mapToAlign: RangeMap[BigInteger, Record], alignIpv4: Boolean = false): List[Record] = mapToAlign.asMapOfRanges().asScala.toList
    .flatMap {
      case (key, _) if empty(key) => List()
      case (range, record: Ipv4Record) if alignIpv4 =>
        record.splitPrefixes(range).map { ipv6 =>
          record.updateRange(RecordRange.from(ipv6).key)
        }
      case (range, record: Ipv6Record) =>
        record.splitPrefixes(range).map { ipv6 =>
          record.updateRange(RecordRange.from(ipv6).key)
        }
      case (range, record) => List(record.updateRange(range))
    }

  def empty(key: Range[BigInteger]): Boolean = key.canonical(DiscreteDomain.bigIntegers()).isEmpty

  def toInterval(r: Range[BigInteger]): (BigInteger, BigInteger) = {
    val start = if (r.lowerBoundType() == BoundType.CLOSED) {
      r.lowerEndpoint()
    } else {
      r.lowerEndpoint().add(BigInteger.ONE)
    }
    val end = if (r.upperBoundType() == BoundType.CLOSED) {
      r.upperEndpoint()
    } else {
      r.upperEndpoint().subtract(BigInteger.ONE)
    }
    (start, end)
  }

  def size(r: Range[BigInteger]): BigInteger = {
    val (start, end) = toInterval(r)
    end.subtract(start).add(BigInteger.ONE)
  }

}
