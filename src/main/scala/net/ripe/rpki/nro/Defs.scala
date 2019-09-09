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

  // Exposing copy so that we can do generic update operations on Record
  trait Updates[A] {
    def status_(a: A, status: String): A
    def ext_(a: A, ext: String): A
    def oid_(a: A, oid: String): A
    def date_(a: A, date: String): A
    def cc_(a: A, cc: String): A
  }

  implicit class UpdateOps[A](a: A)(implicit ev: Updates[A]) {
    def status_(status: String) = ev.status_(a, status)
    def ext_(status: String)    = ev.ext_(a, status)
    def oid_(status: String)    = ev.oid_(a, status)
    def date_(status: String)   = ev.date_(a, status)
    def cc_(status: String)     = ev.cc_(a, status)
  }

}
