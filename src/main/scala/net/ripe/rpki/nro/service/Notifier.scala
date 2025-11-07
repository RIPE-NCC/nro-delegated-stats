package net.ripe.rpki.nro.service

import courier.Defaults._
import courier._
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Const.RSCG
import net.ripe.rpki.nro.model.{Conflict, Record, Unclaimed, WithKey}
import net.ripe.rpki.nro.{Const, Logging}

import scala.collection.immutable.ArraySeq
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class Notifier(mailer: Mailer, allowedList: Seq[Record]) extends Logging {

  def isAllowed(c: Conflict): Boolean = {
    val check = allowedList.contains(c.a) || allowedList.contains(c.b)
    if (check)
      logger.info(s"$c is allowed")
    else
      logger.info(s"$c is not allowed")
    check
  }

  private def findStickyIssues[T <: WithKey](previous: Seq[T], current: Seq[T]): Set[T] = {
    val previousMap = previous.map(c => c.key -> c).toMap
    current.map(_.key)
      .toSet
      .intersect(previousMap.keySet)
      .map(previousMap)
  }

  def findStickyConflicts(previous: Seq[Conflict], current: Seq[Conflict]): Set[Conflict] =
    findStickyIssues(previous, current).filterNot(isAllowed)

  def findStickyUnclaimed(previous: Seq[Unclaimed], current: Seq[Unclaimed]): Set[Unclaimed] =
    findStickyIssues(previous, current)

  def notifyOnIssues(conflicts: Set[Conflict], unclaimed: Set[Unclaimed]): Unit = {
    if (conflicts.nonEmpty || unclaimed.nonEmpty) {
      val envelope: Envelope = createEnvelope(conflicts, unclaimed)
      Await.result(mailer(envelope), Duration.Inf)
    }
  }

  def createEnvelope(conflicts: Set[Conflict], unclaimed: Set[Unclaimed]): Envelope = {
    val unclaimedText = {
      if (unclaimed.nonEmpty)
        s"Please verify the following unclaimed resources:\n\n${unclaimed.mkString("\n")}"
      else ""
    }

    val conflictText = {
      if (conflicts.nonEmpty)
        s"Please verify the following resource conflicts:\n\n${conflicts.mkString("\n\n--\n\n")}"
      else ""
    }

    val messageText = List(conflictText, unclaimedText).filter(_.nonEmpty).mkString("\n\n")

    val sendTo = {
      val registries =
        conflicts.flatMap(c => Set(c.a.registry, c.b.registry)) ++
          unclaimed.flatMap(c => Set(c.record.registry))

      (registries.filter(_ != Const.IANA) ++ Set(RSCG)).map(contacts)
    }.filter(a => a != null && a.nonEmpty)
      .toArray

    Envelope
      .from(sender.addr)
      .to(ArraySeq.unsafeWrapArray(sendTo.map(_.addr)): _*)
      .subject(s"There are problematic delegated stats since ${config.PREV_CONFLICT_DAY}")
      .content(Text(messageText))
  }
}
