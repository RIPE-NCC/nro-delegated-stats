package net.ripe.rpki.nro.main

import java.math.BigInteger

import com.google.common.collect.{BoundType, DiscreteDomain, Range, RangeMap, TreeRangeMap}
import net.ripe.rpki.nro.model.{Ipv6Record, Record, RecordRange}
import scala.jdk.CollectionConverters._
/**
 * Range and RangeMap related utilities
 */
trait Ranges {

  def asRangeMap(recs: List[Record]): RangeMap[BigInteger, Record] = {
    val result: RangeMap[BigInteger, Record] = TreeRangeMap.create[BigInteger, Record]()
    recs.foreach(rec => result.put(rec.range.key, rec))
    result
  }

  def updateRecordRange(currentMap: RangeMap[BigInteger, Record]): List[Record] = currentMap.asMapOfRanges().asScala.toList
    .flatMap {
      case (key, _) if empty(key) => List()
      case (range, record: Ipv6Record) =>
        record.splitPrefixes(range).map { ipv6 =>
          record.update(RecordRange.from(ipv6).key)
        }
      case (range, record) => List(record.update(range))
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
