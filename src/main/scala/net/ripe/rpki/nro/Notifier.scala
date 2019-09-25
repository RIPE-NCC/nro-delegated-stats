package net.ripe.rpki.nro

import java.io.File

import courier.Defaults._
import courier._
import org.slf4j.LoggerFactory
import Settings._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class Notifier(mailer: Mailer) {

  val logger = LoggerFactory.getLogger(this.getClass)

  def notifyConflicts(current: List[Conflict], previous: List[Conflict]): String = {

    // Conflict records contains date,  we need to convert to keys without dates
    // to find persisting conflicts i.e intersection of previous and current conflicts
    val currentMap = current.map(c => c.key -> c).toMap
    val previousMap = previous.map(c => c.key -> c).toMap

    val stickyConflicts = currentMap.keySet.intersect(previousMap.keySet).map(previousMap)

    if(stickyConflicts.nonEmpty) {
      logger.info("Found sticky conflicts from previous time")
      stickyConflicts.foreach(c => logger.info(c.toString))
      sendConflicts(conflicts = stickyConflicts)
      "Sent some conflict"
    } else
      "No conflicts, no mail"

  }

  def sendConflicts(conflicts: Set[Conflict])={
    val rsContacts = conflicts.flatMap(c => Set(c.a.registry, c.b.registry)).filter(_ != "iana").map(contacts).toArray
    val envelope: Envelope = Envelope
      .from(sender.addr)
      .to(rsContacts.map(_.addr):_*)
      .subject(s"There are conflicting delegated stats since $PREV_CONFLICT_DAY")
      .content(Text(s"Please verify the following conflicts:\n\n${conflicts.mkString("\n\n--\n\n")}"))
    Await.result(mailer(envelope), Duration.Inf)
  }

}


