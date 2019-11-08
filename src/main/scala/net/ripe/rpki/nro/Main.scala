package net.ripe.rpki.nro

import java.time.LocalDate

import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.main.Stats
import net.ripe.rpki.nro.service.{Notifier, Ports}
import scala.util.Properties

object Main extends Stats with App {

  var startDate = Properties.propOrNone("startDate").map(LocalDate.parse).getOrElse(LocalDate.now)
  val endDate   = Properties.propOrNone("endDate").map(LocalDate.parse).getOrElse(LocalDate.now)


  // If start and end date is not given they will be today and this should run once.
  while(startDate.compareTo(endDate) <= 0) {

    Configs.config = new Configs(startDate)
    logger.info("Data dir: "+Configs.config.todayDataDirectory)
    logger.info("Result dir: "+Configs.config.todayResultDirectory)

    val (rirRecords, ianaRecord, previousResult, previousConflicts) = Ports.fetchAndParse()
    val (results, mergedResults, currentConflicts, unclaimed, overclaimed) = process(rirRecords, ianaRecord, previousResult, previousConflicts)

    val notifier  = new Notifier(mailer)
    notifier.notifyConflicts(currentConflicts, previousConflicts)

    Ports.writeRecords(results, config.currentResultFile)
    Ports.writeRecords(mergedResults, config.currentMergedFile)
    Ports.writeConflicts(currentConflicts, config.currentConflictFile)

    Ports.writeClaims(unclaimed, config.currentUnclaimedFile)
    Ports.writeClaims(overclaimed, config.currentOverclaimedFile)

    startDate = startDate.plusDays(1)
  }
}
