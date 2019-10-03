package net.ripe.rpki.nro

import java.math.BigInteger
import com.google.common.collect._
import scala.jdk.CollectionConverters._

/**
 * Range and RangeMap related utilities
 */
object Ranges {

  def asRangeMap(recs: List[Record]): RangeMap[BigInteger, Record] = {
    val result: RangeMap[BigInteger, Record] = TreeRangeMap.create[BigInteger, Record]()
    recs.foreach(rec => result.put(rec.range.key, rec))
    result
  }

  def updateMap(currentMap: RangeMap[BigInteger, Record]): List[Record] = currentMap.asMapOfRanges().asScala.toList
    .flatMap {
      case (key, _) if empty(key) => List()
      case (range, record: Ipv6Record) =>
        Ipv6Record.splitPrefixes(range).map { ipv6 =>
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

  def length(r: Range[BigInteger]): BigInteger = {
    val (start, end) = toInterval(r)
    end.subtract(start).add(BigInteger.ONE)
  }

}
