package net.ripe.rpki.nro.service

import courier.Defaults._
import courier._
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Const.RSCG
import net.ripe.rpki.nro.model.{Conflict, Record, Unclaimed, WithKey}
import net.ripe.rpki.nro.{Const, Logging}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.collection.immutable.ArraySeq

class Notifier(mailer: Mailer, allowedList : Seq[Record]) extends Logging {

  def isAllowed(c: Conflict): Boolean = {
    val check = allowedList.contains(c.a) || allowedList.contains(c.b)
    if (check)
      logger.info(s"$c is allowed")
    else
      logger.info(s"$c is not allowed")
    check
  }

  private def findStickyIssues[T <: WithKey](current: Seq[T], previous: Seq[T]): Set[T] = {
    val previousMap = previous.map(c => c.key -> c).toMap
    current.map(_.key)
      .toSet
      .intersect(previousMap.keySet)
      .map(previousMap)
  }

  def findStickyConflicts(current: Seq[Conflict], previous: Seq[Conflict]): Set[Conflict] =
    findStickyIssues(current, previous).filterNot(isAllowed)

  def findStickyUnclaimed(current: Seq[Unclaimed], previous: Seq[Unclaimed]): Set[Unclaimed] =
    findStickyIssues(current, previous)

  def notifyOnIssues(conflicts: Set[Conflict], unclaimed: Set[Unclaimed]): Unit = {
    val rsContactsFromConflicts: Array[String] =
      conflicts.flatMap(c => Set(c.a.registry, c.b.registry))
        .filter(_ != Const.IANA)
        .map(contacts)
        .toArray :+ contacts(RSCG)

    val envelope: Envelope = Envelope
      .from(sender.addr)
      .to(ArraySeq.unsafeWrapArray(rsContactsFromConflicts.map(_.addr)): _*)
      .subject(s"There are conflicting delegated stats since ${config.PREV_CONFLICT_DAY}")
      .content(Text(s"Please verify the following conflicts:\n\n${conflicts.mkString("\n\n--\n\n")}"))

    Await.result(mailer(envelope), Duration.Inf)
  }
}
