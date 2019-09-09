package net.ripe.rpki.nro 

// Exposing copy so that we can do generic update operations on Record (Asn, Ipv4, Ipv6)
trait Updates[A] {
    def status_(a: A, status: String): A
    def ext_(a: A, ext: String): A
    def oid_(a: A, oid: String): A
    def date_(a: A, date: String): A
    def cc_(a: A, cc: String): A
}

object Updates {
  implicit class UpdateOps[A](a: A)(implicit ev: Updates[A]) {
    def status_(status: String): A = ev.status_(a, status)
    def ext_(ext: String): A = ev.ext_(a, ext)
    def oid_(oid: String): A = ev.oid_(a, oid)
    def date_(date: String): A = ev.date_(a, date)
    def cc_(cc: String): A = ev.cc_(a, cc)
  }
  implicit val updateAsns: Updates[AsnRecord] = new Updates[AsnRecord] {
    def status_(r: AsnRecord, status: String): AsnRecord = r.copy(status = status)
    def ext_(r: AsnRecord, ext: String): AsnRecord = r.copy(ext = ext)
    def oid_(r: AsnRecord, oid: String): AsnRecord = r.copy(oid = oid)
    def cc_(r: AsnRecord, oid: String): AsnRecord = r.copy(cc = oid)
    def date_(r: AsnRecord, oid: String): AsnRecord = r.copy(date = oid)
  }
  implicit val updateIpv6s: Updates[Ipv6Record] = new Updates[Ipv6Record] {
    def status_(r: Ipv6Record, status: String): Ipv6Record = r.copy(status = status)
    def ext_(r: Ipv6Record, ext: String): Ipv6Record = r.copy(ext = ext)
    def oid_(r: Ipv6Record, oid: String): Ipv6Record = r.copy(oid = oid)
    def cc_(r: Ipv6Record, cc: String): Ipv6Record = r.copy(cc = cc)
    def date_(r: Ipv6Record, date: String): Ipv6Record = r.copy(date = date)
  }
  implicit val updateIpv4s: Updates[Ipv4Record] = new Updates[Ipv4Record] {
    def status_(r: Ipv4Record, status: String): Ipv4Record = r.copy(status = status)
    def ext_(r: Ipv4Record, ext: String): Ipv4Record = r.copy(ext = ext)
    def oid_(r: Ipv4Record, oid: String): Ipv4Record = r.copy(oid = oid)
    def cc_(r: Ipv4Record, cc: String): Ipv4Record = r.copy(cc = cc)
    def date_(r: Ipv4Record, date: String): Ipv4Record = r.copy(date = date)
  }
}
