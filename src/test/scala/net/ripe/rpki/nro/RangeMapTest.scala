package net.ripe.rpki.nro

import com.google.common.collect.{Range, RangeMap, TreeRangeMap}
import org.scalatest.FlatSpec

import scala.jdk.CollectionConverters._

// Illustrating how Range Map behaves.
// [A..B] means closed interval including A and B
// (A..B] open interval at the beginning so for integers it will be from A+1 up to and B
// (A..B) open interval at the beginning so for integers it will be from A+1 up to and B-1
class RangeMapTest extends FlatSpec{

  val map : RangeMap[Integer,String] = TreeRangeMap.create[Integer,String]()

  "Range maps examples to illustrate how maps work " should "be able to add non overlapping ranges" in {
      map.put(Range.closed( 1,10), "A")
      map.put(Range.closed(11,20), "B")
      map.put(Range.closed(21,30), "C")

      assert(map.asMapOfRanges().asScala.mkString(", ") == "[1..10] -> A, [11..20] -> B, [21..30] -> C")
  }

  it should "replace and exact overlaps" in {
    map.put(Range.closed(21,30), "D")
    assert(map.asMapOfRanges().asScala.mkString(", ") == "[1..10] -> A, [11..20] -> B, [21..30] -> D")
  }

  it should "split in case there is a partial overlap" in {
    map.put(Range.closed(21,25), "E")
    assert(map.asMapOfRanges().asScala.mkString(", ") == "[1..10] -> A, [11..20] -> B, [21..25] -> E, (25..30] -> D")
  }

  it should "replace ranges overlapped by new bigger range" in {
    map.put(Range.closed(1,20), "AB")
    assert(map.asMapOfRanges().asScala.mkString(", ") == "[1..20] -> AB, [21..25] -> E, (25..30] -> D")
  }

  it can "sometimes produces empty interval when splitting" in {
    map.put(Range.closed( 1,10), "WEIRD1")
    map.put(Range.closed(11,20), "WEIRD2")
    assert(map.asMapOfRanges().asScala.mkString(", ") ==
      "[1..10] -> WEIRD1, (10..11) -> AB, [11..20] -> WEIRD2, [21..25] -> E, (25..30] -> D")
    // (10..11) is actually an empty interval.
  }

  it can "remove ranges" in {
    map.remove(Range.closed(1,10))
    map.remove(Range.open(10,11))
    map.remove(Range.closed(11,20))
    assert(map.asMapOfRanges().asScala.mkString(", ") ==
      "[21..25] -> E, (25..30] -> D")

  }

  it can "remove partial ranges" in {
    map.remove(Range.closed(21,23))
    assert(map.asMapOfRanges().asScala.mkString(", ") ==
      "(23..25] -> E, (25..30] -> D")
  }

}
