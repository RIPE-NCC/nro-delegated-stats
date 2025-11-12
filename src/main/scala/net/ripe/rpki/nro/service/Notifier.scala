package net.ripe.rpki.nro.service

import courier.Defaults._
import courier._
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Const.RSCG
import net.ripe.rpki.nro.model.{Conflict, Record, Unclaimed, WithKey}
import net.ripe.rpki.nro.{Const, Logging}

import scala.collection.immutable.ArraySeq
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Notifier {
  val CONFLICTS = "conflicts"
  val UNCLAIMED = "unclaimed"
  val BOTH_CONFLICTS_AND_UNCLAIMED = "both"
}

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
      val envelopes = createEnvelopes(conflicts, unclaimed)
      val all = Future.sequence(envelopes.values.map(x => mailer(x)))
      Await.result(all, Duration.Inf)
    }
  }

  def createEnvelopes(conflicts: Set[Conflict], unclaimed: Set[Unclaimed]): Map[String, Envelope] = {
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

    def contactSet(x: Set[String]) =
      (x.filter(_ != Const.IANA) ++ Set(RSCG))
        .map(contacts)
        .filter(a => a != null && a.nonEmpty)

    def envelope(sendTo: Set[String], subject: String, messageText: String) =
      Envelope
        .from(sender.addr)
        .to(ArraySeq.unsafeWrapArray(sendTo.map(_.addr).toArray): _*)
        .subject(subject)
        .content(Text(messageText))

    val conflictContacts = contactSet(conflicts.flatMap(c => Set(c.a.registry, c.b.registry)))
    val unclaimedContacts = contactSet(unclaimed.flatMap(c => Set(c.record.registry)))

    val both = conflictContacts.intersect(unclaimedContacts)
    val conflictsOnly = conflictContacts.diff(both)
    val unclaimedOnly = unclaimedContacts.diff(both)

    Seq(
      (Notifier.CONFLICTS, conflictsOnly, envelope(conflictsOnly, s"There are conflicting delegated stats since ${config.PREV_CONFLICT_DAY}", conflictText)),
      (Notifier.UNCLAIMED, unclaimedOnly, envelope(unclaimedOnly, s"There are unclaimed delegated stats since ${config.PREV_CONFLICT_DAY}", unclaimedText)),
      (Notifier.BOTH_CONFLICTS_AND_UNCLAIMED, both, envelope(both, s"There are problematic delegated stats since ${config.PREV_CONFLICT_DAY}", messageText))
    ).filter(_._2.nonEmpty)
      .map { case (key, _, env) => key -> env }
      .toMap
  }
}
