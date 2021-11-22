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

import scala.collection.immutable.ArraySeq

class Notifier(mailer: Mailer, allowedList : List[Record]) extends Logging {

  def isAllowed(c: Conflict): Boolean = {
    val check = allowedList.contains(c.a) || allowedList.contains(c.b)
    if (check)
      logger.info(s"$c is allowed")
    else
      logger.info(s"$c is not allowed")
    check
  }

  def findStickyConflicts(current: List[Conflict], previous: List[Conflict]): Set[Conflict] = {
    // Conflict records contains date,  we need to convert to keys without dates
    // to find persisting conflicts i.e intersection of previous and current conflicts
    val currentKeys = current.map(_.key).toSet
    val previousMap = previous.map(c => c.key -> c).toMap

    val stickyConflicts = currentKeys
      .intersect(previousMap.keySet)
      .map(previousMap)
      .filterNot(isAllowed)
    stickyConflicts
  }

  def notifyConflicts(conflicts: Set[Conflict]): Boolean = {
    if (conflicts.isEmpty) false
    else {
      val rsContactsFromConflicts: Array[String] = conflicts.flatMap(c => Set(c.a.registry, c.b.registry))
        .filter(_ != Const.IANA)
        .map(contacts).toArray :+ contacts(RSCG)
      val envelope: Envelope = Envelope
        .from(sender.addr)
        .to(ArraySeq.unsafeWrapArray(rsContactsFromConflicts.map(_.addr)): _*)
        .subject(s"There are conflicting delegated stats since ${config.PREV_CONFLICT_DAY}")
        .content(Text(s"Please verify the following conflicts:\n\n${conflicts.mkString("\n\n--\n\n")}"))

      Await.result(mailer(envelope), Duration.Inf)
      true
    }

  }
}


