package net.ripe.rpki.nro

import net.ripe.ipresource.{IpResource, IpResourceRange, IpResourceSet}

import scala.collection.JavaConverters._
import scala.collection.parallel.ParIterable
import scala.collection.{Iterable, Iterator, SortedMap}

import Defs._

object Merger {

  def checkAndMerge(ma: SortedMap[IpResourceRange, Record], mb: SortedMap[IpResourceRange, Record]) = {
    val ka       = ma.keySet
    val kb       = mb.keySet
    val conflict = ka.intersect(kb)
    conflict.foreach(k => {
      val va = ma(k)
      val vb = mb(k)

      println(s"$k is found both on ${va.registry} and ${vb.registry}")
      println(s"<    $va ")
      println(s">    $vb ")
    })
    ma ++ mb
  }

  // Combining multiple maps from different RIRs
  def combine(resourceMap: ParIterable[SortedMap[IpResourceRange, Record]]): SortedMap[IpResourceRange, Record] =
    resourceMap.foldLeft(SortedMap[IpResourceRange, Record]())(checkAndMerge)

  def substractSpace(slash0: IpResourceRange, used : Iterable[IpResourceRange]): Iterator[IpResource] = {
    val allSpace = new IpResourceSet(slash0)
    used.foreach(allSpace.remove)
    allSpace.iterator.asScala
  }
}
