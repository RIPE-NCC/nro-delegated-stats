package net.ripe.rpki.nro

import net.ripe.rpki.nro.Settings._

object Main extends Stats with App {

  val (rirRecords, ianaRecord, previousConflicts) = Ports.fetchAndParse()

  val (results, mergedResults, currentConflicts, unclaimed, overclaimed) = process(rirRecords, ianaRecord, previousConflicts)

  val notifier = new Notifier(Settings.mailer)
  notifier.notifyConflicts(currentConflicts, previousConflicts)

  Ports.writeRecords(results, currentResultFile)
  Ports.writeRecords(mergedResults, currentMergedFile)
  Ports.writeConflicts(currentConflicts, currentConflictFile)

  Ports.writeClaims(unclaimed, currentUnclaimedFile)
  Ports.writeClaims(overclaimed, currentOverclaimedFile)

}
