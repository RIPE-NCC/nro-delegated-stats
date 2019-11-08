package net.ripe.rpki.nro.model

import net.ripe.rpki.nro.Configs.config
import net.ripe.rpki.nro.Const.{ALLOCATED, ASSIGNED, AVAILABLE, DEFAULT_CC, IANA, IETF, RESERVED}
import net.ripe.rpki.nro.main.Ranges

case class Records(asn: List[Record], ipv4: List[Record], ipv6: List[Record]) extends Ranges {

  def fixUnclaimed: Records = {
    def fix[R]: Record => Record = (rec: Record) => {
      val info = rec.stat.copy(date = config.TODAY, registry = rec.stat.status, status = AVAILABLE)
      rec.update(info)
    }

    fixRecords(fix)
  }

  def fixIana: Records = {
    def fix[R]: Record => Record = (rec: Record) => rec.stat.status match {
      case IETF => rec.update(rec.stat.copy(oid = IETF, ext = IANA))
      case IANA => rec.update(rec.stat.copy(oid = IANA, status = ASSIGNED, ext = IANA))
      case _ => rec.update(rec.stat.copy(ext = IANA))
    }

    fixRecords(fix)
  }

  def fixRIRs: Records = {
    def fix: Record => Record = (rec: Record) => rec.stat.status match {
      // RESERVED and AVAILABLE has neither date nor country (except AFRINIC with ZZ)
      case RESERVED | AVAILABLE => rec.update(rec.stat.copy(date = config.TODAY, cc = DEFAULT_CC))
      // Whatever allocated in original RIR file appears as ASSIGNED
      case ALLOCATED => rec.update(rec.stat.copy(status = ASSIGNED))
      case _ => rec
    }

    fixRecords(fix)
  }

  def fixRecords(fix: Record => Record): Records = Records(
    this.asn.map(fix),
    this.ipv4.map(fix),
    this.ipv6.map(fix))

  def substract(that: Records): Records = {
    val (thisAsn, thisIpv4, thisIpv6) = (asRangeMap(this.asn), asRangeMap(this.ipv4), asRangeMap(this.ipv6))
    val (thatAsn, thatIpv4, thatIpv6) = (asRangeMap(that.asn), asRangeMap(that.ipv4), asRangeMap(that.ipv6))

    thatAsn.asMapOfRanges().forEach { case (key, _) => thisAsn.remove(key) }
    thatIpv4.asMapOfRanges().forEach { case (key, _) => thisIpv4.remove(key) }
    thatIpv6.asMapOfRanges().forEach { case (key, _) => thisIpv6.remove(key) }

    Records(alignRecordWithMapRangeKeys(thisAsn), alignRecordWithMapRangeKeys(thisIpv4), alignRecordWithMapRangeKeys(thisIpv6))
  }

  def append(that: Records): Records =
    Records(this.asn ++ that.asn, this.ipv4 ++ that.ipv4, this.ipv6 ++ that.ipv6)

  def sorted(): Records =
    Records(this.asn.sorted, this.ipv4.sorted, this.ipv6.sorted)

  def partition(criteria: Record => Boolean): (Records, Records) = {
    val (asnRirs, asnNonRirs) = this.asn.partition(criteria)
    val (ipv4Rirs, ipv4NonRirs) = this.ipv4.partition(criteria)
    val (ipv6Rirs, ipv6NonRirs) = this.ipv6.partition(criteria)

    (Records(asnRirs, ipv4Rirs, ipv6Rirs), Records(asnNonRirs, ipv4NonRirs, ipv6NonRirs))
  }

  def size: Int = asn.size + ipv4.size + ipv6.size
}