package net.ripe.rpki.nro.service

import courier.Defaults._
import courier._
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Const.RSCG
import net.ripe.rpki.nro.model.Conflict
import net.ripe.rpki.nro.{Const, Logging}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import net.ripe.rpki.nro.model.Record

class Notifier(mailer: Mailer, whiteList: List[Record]) extends Logging {

  def whiteListed(c : Conflict) = {
      val check = whiteList.contains(c.a) || whiteList.contains(c.b) 
      if(check)
        logger.info(s"$c is whitelisted")
      else
        logger.info(s"$c is not whitelisted")
      check
  }

  def notifyConflicts(current: List[Conflict], previous: List[Conflict]): String = {

    // Conflict records contains date,  we need to convert to keys without dates
    // to find persisting conflicts i.e intersection of previous and current conflicts
    val currentKeys = current.map(_.key).toSet
    val previousMap = previous.map(c => c.key -> c).toMap

    val stickyConflicts = currentKeys
      .intersect(previousMap.keySet)
      .map(previousMap)
      .filterNot(whiteListed)

    if(stickyConflicts.nonEmpty) {
      logger.info("Found sticky conflicts from previous time")
      stickyConflicts.foreach(c => logger.info(c.toString))
      sendConflicts(conflicts = stickyConflicts)
      "Sent some conflict"
    } else{
      logger.info("No conflicts, no mail")
      "No conflicts, no mail"
    }


  }

  def sendConflicts(conflicts: Set[Conflict]): Unit ={
    var rsContacts: Array[String] = conflicts.flatMap(c => Set(c.a.registry, c.b.registry)).filter(_ != Const.IANA).map(contacts).toArray :+ contacts(RSCG)
    val envelope: Envelope = Envelope
      .from(sender.addr)
      .to(rsContacts.map(_.addr):_*)
      .subject(s"There are conflicting delegated stats since ${config.PREV_CONFLICT_DAY}")
      .content(Text(s"Please verify the following conflicts:\n\n${conflicts.mkString("\n\n--\n\n")}"))
    Await.result(mailer(envelope), Duration.Inf)
  }

}


