package net.ripe.rpki.nro

import java.io.{File, StringReader}

import com.github.tototoshi.csv.CSVReader
import net.ripe.rpki.nro.Ports._
import org.scalatest.FlatSpec

import scala.collection.immutable

class PortsTest extends FlatSpec with TestUtil {

  "Parsing delegated " should "work for partial apnic data" in {
    val apnic = parseRecordFile(getClass.getResource("/data/apnic").getFile)
    assert(apnic.asn.size == 100)
    assert(apnic.ipv4.size == 100)
    assert(apnic.ipv6.size == 100)
  }

  it should "work for partial iana data" in {
    val iana = parseRecordFile(getClass.getResource("/data/iana").getFile)
    assert(iana.asn.size == 6)
    assert(iana.ipv4.size == 6)
    assert(iana.ipv6.size == 6)
  }

  it should "deserialize conflict in proper location " in {

    val ripe =
      """|ripencc|AU|ipv4|1.10.10.0|256|20110811|assigned|A9173591
         |ripencc|CN|ipv4|1.10.16.0|4096|20110412|allocated|A92319D5""".stripMargin

    val apnic =
      """|apnic|AU|ipv4|1.10.10.0|256|20110811|assigned|A9173591
         |apnic|CN|ipv4|1.10.11.0|256|20110414|allocated|A92E1062""".stripMargin

    val ripeRecs = toRecords(ripe)
    val apnicRecs = toRecords(apnic)

    val original = ripeRecs.zip(apnicRecs).map { case (a, b) => Conflict(a, b) }
    writeConflicts(original, "output")

    val readBack = readConflicts("output")

    assert(readBack == original)

  }

  it should "deserialize empty conflicts" in {
    val noConflict = List[Conflict]()
    val tempOutput = File.createTempFile("conflict","txt")
    writeConflicts(noConflict, tempOutput.toString)
    val readBack = readConflicts(tempOutput.toString)
    assert(readBack == noConflict)

  }


}