package net.ripe.rpki.nro

import net.ripe.rpki.nro.Settings._
import net.ripe.rpki.nro.main.Stats
import net.ripe.rpki.nro.service.{Notifier, Ports}

object Main extends Stats with App {

  val (rirRecords, ianaRecord, previousResult, previousConflicts) = Ports.fetchAndParse()

  val (results, mergedResults, currentConflicts, unclaimed, overclaimed) = process(rirRecords, ianaRecord, previousResult, previousConflicts)

  val notifier = new Notifier(Settings.mailer)
  notifier.notifyConflicts(currentConflicts, previousConflicts)

  Ports.writeRecords(results, currentResultFile)
  Ports.writeRecords(mergedResults, currentMergedFile)
  Ports.writeConflicts(currentConflicts, currentConflictFile)

  Ports.writeClaims(unclaimed, currentUnclaimedFile)
  Ports.writeClaims(overclaimed, currentOverclaimedFile)

}
