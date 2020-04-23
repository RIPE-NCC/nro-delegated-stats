package net.ripe.rpki.nro.model

import net.ripe.rpki.nro.TestUtil
import net.ripe.rpki.nro.service.Ports._
import org.scalatest.FlatSpec
import net.ripe.rpki.nro.Const._
import net.ripe.rpki.nro.Configs.config

class RecordsFormatTest extends FlatSpec with TestUtil {

  "Records formatting" should " format IANA " in {
    val iana  = parseRecordFile(getResourceFile("/data/iana")).formatIana
    val stats = iana.asn.map(_.stat) ++ iana.ipv4.map(_.stat) ++ iana.ipv6.map(_.stat)

    stats.foreach( stat ⇒
        assert(
          (stat.status == RESERVED && stat.oid == IETF) ||
          (stat.status == ASSIGNED && stat.oid == IANA) ||
          stat.ext == IANA))
  }

  it should "format Unclaimed " in {
    val unclaimed = parseRecordFile(getResourceFile("/data/iana")).formatUnclaimed
    val stats = unclaimed.asn.map(_.stat) ++ unclaimed.ipv4.map(_.stat) ++ unclaimed.ipv6.map( _.stat)

    stats.foreach( stat ⇒
        assert(
          stat.date == config.CURRENT_DAY &&
          stat.status == AVAILABLE
      ))
  }

  it should "format RIRs" in {
    val ripe  = parseRecordFile(getResourceFile("/data/arin")).formatRIRs
    val stats = ripe.asn.map(_.stat) ++ ripe.ipv4.map(_.stat) ++ ripe.ipv6.map( _.stat)

    stats.foreach { stat ⇒
      if (stat.status == RESERVED || stat.status == AVAILABLE) {
        assert(stat.date == config.CURRENT_DAY && stat.cc == DEFAULT_CC && stat.oid == stat.registry)
      }
      assert(stat.status != ALLOCATED)
    }
  }
}
