package net.ripe.rpki.nro

import java.math.BigInteger
import net.ripe.ipresource.{IpAddress, IpResourceRange, IpResourceType}
import scala.collection.SortedMap

object Defs {

  type Line = Array[String]
  val TODAY = java.time.LocalDate.now.toString.replaceAll("-", "")

  implicit val ipResourceRangeOrder = new Ordering[IpResourceRange] {
    override def compare(a: IpResourceRange, b: IpResourceRange) = a.compareTo(b)
  }

  def rangeLen(r: IpResourceRange) =
    r.getEnd.getValue.subtract(r.getStart.getValue).add(BigInteger.ONE)
  
}



