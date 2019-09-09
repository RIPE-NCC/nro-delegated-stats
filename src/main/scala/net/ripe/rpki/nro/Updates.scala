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
    def status_(status: String) = ev.status_(a, status)
    def ext_(status: String)    = ev.ext_(a, status)
    def oid_(status: String)    = ev.oid_(a, status)
    def date_(status: String)   = ev.date_(a, status)
    def cc_(status: String)     = ev.cc_(a, status)
  }
  implicit val updateAsns = new Updates[AsnRecord] {
    def status_(r: AsnRecord, status: String) = r.copy(status = status)
    def ext_(r: AsnRecord, ext: String)       = r.copy(ext = ext)
    def oid_(r: AsnRecord, oid: String)       = r.copy(oid = oid)
    def cc_(r: AsnRecord, oid: String)        = r.copy(cc = oid)
    def date_(r: AsnRecord, oid: String)      = r.copy(date = oid)
  }
  implicit val updateIpv6s = new Updates[Ipv6Record] {
    def status_(r: Ipv6Record, status: String) = r.copy(status = status)
    def ext_(r: Ipv6Record, ext: String)       = r.copy(ext = ext)
    def oid_(r: Ipv6Record, oid: String)       = r.copy(oid = oid)
    def cc_(r: Ipv6Record, cc: String)         = r.copy(cc = cc)
    def date_(r: Ipv6Record, date: String)     = r.copy(date = date)
  }
  implicit val updateIpv4s = new Updates[Ipv4Record] {
    def status_(r: Ipv4Record, status: String) = r.copy(status = status)
    def ext_(r: Ipv4Record, ext: String)       = r.copy(ext = ext)
    def oid_(r: Ipv4Record, oid: String)       = r.copy(oid = oid)
    def cc_(r: Ipv4Record, cc: String)         = r.copy(cc = cc)
    def date_(r: Ipv4Record, date: String)     = r.copy(date = date)
  }
}
