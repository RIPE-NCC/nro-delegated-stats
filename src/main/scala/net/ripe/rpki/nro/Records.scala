package net.ripe.rpki.nro

import net.ripe.rpki.nro.Defs._
import net.ripe.rpki.nro.Updates._

// Records holder for three type of records, contains some logic of fixing entries
case class Records(
                    source: String,
                    header: Line,
                    summaries: List[Line],
                    asn: List[AsnRecord],
                    ipv4: List[Ipv4Record],
                    ipv6: List[Ipv6Record]
                  ) {

  def fixIana: Records = {
    def fix[A <: Record : Updates]: A => A = (rec: A) => rec.status match {
      case IETF => rec.oid_(IETF).ext_(IANA)
      case IANA => rec.oid_(IANA).status_(ASSIGNED).ext_(IANA)
      case _ => rec.ext_(IANA)
    }

    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }

  // Reserved and available records normally have neither country code or date, when combined it will be filled with ZZ and today's date.
  def fixRIRs: Records = {
    def fix[A <: Record : Updates]: A => A = (rec: A) => rec.status match {
      case RESERVED | AVAILABLE => rec.date_(TODAY).cc_(DEFAULT_CC)
      case ALLOCATED => rec.status_(ASSIGNED)
      case _ => rec
    }

    this.asn_(fix).ipv4_(fix).ipv6_(fix)
  }


  override def toString: String =
    s"""Source: $source \nHeader: ${header.mkString(",")} \nSummaries: \n${
      summaries
        .map(_.mkString(","))
        .mkString("\n")
    }\n\n"""

  // Map values wrapper
  def asn_(f: AsnRecord => AsnRecord)(implicit ev: Updates[AsnRecord]): Records =
    this.copy(asn = this.asn.map(f))

  def ipv4_(f: Ipv4Record => Ipv4Record)(implicit ev: Updates[Ipv4Record]): Records =
    this.copy(ipv4 = this.ipv4.map(f))

  def ipv6_(f: Ipv6Record => Ipv6Record)(implicit ev: Updates[Ipv6Record]): Records =
    this.copy(ipv6 = this.ipv6.map(f))

}

object Records {

  def combineResources(resourceMap: Iterable[List[Record]]): (List[Record], List[Conflict]) = {

    val sortedRecords = resourceMap.reduce(_ ++ _).sorted

    var result : List[Record]  = Nil 
    var conflicts : List[Conflict] = Nil 
    var lastRecord = sortedRecords.head 

    sortedRecords.tail foreach { nextRecord => 
        if(lastRecord.endsAfter(nextRecord)) {
            conflicts = Conflict(lastRecord, nextRecord) :: conflicts
        } else {
            result = lastRecord :: result 
            lastRecord = nextRecord
        } 
    }

    ((lastRecord::result).reverse, conflicts.reverse)
  }

  def mergeSiblings(records: List[Record]): List[Record] = {

    val sortedRecords = records.sorted 

    var result : List[Record] = Nil 
    var lastRecord = sortedRecords.head 

    sortedRecords.tail foreach { nextRecord => 
        if(lastRecord.canMerge(nextRecord)) {
          lastRecord = lastRecord.merge(nextRecord) 
        }
        else {
          result = lastRecord :: result 
          lastRecord = nextRecord
        }
     }
    
     (lastRecord :: result).reverse
  }


}
