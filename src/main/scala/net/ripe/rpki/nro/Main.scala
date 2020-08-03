package net.ripe.rpki.nro

import java.time.LocalDate

import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.main.Stats
import net.ripe.rpki.nro.service.{Notifier, Ports}

import scala.util.{Properties, Try}
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException

object Main extends Stats with App {

  var startDate   = Properties.propOrNone("startDate").map(LocalDate.parse).getOrElse(LocalDate.now)
  val endDate     = Properties.propOrNone("endDate").map(LocalDate.parse).getOrElse(LocalDate.now)
  val ownMagic    = Properties.propOrNone("ownMagic").isDefined
  val environment = Properties.propOrElse("environment", "local")

  configureLogging()

  if (startDate.equals(endDate)) {
    logger.info("Generating stats for a single day ", startDate)
  } else {
    logger.info(s"Generating stats from $startDate to $endDate")
  }

  while (startDate.compareTo(endDate) <= 0) {

    Configs.config = new Configs(startDate)
    logger.info("Data dir: " + Configs.config.currentDataDirectory)
    logger.info("Result dir: " + Configs.config.currentResultDirectory)

    val (rirRecords, ianaRecord, previousResult, previousConflicts, whitelist) = Ports.fetchAndParse(ownMagic)
    val (results, mergedResults, currentConflicts, unclaimed, overclaimed) = process(rirRecords, ianaRecord, previousResult, previousConflicts)

    val notifier = new Notifier(mailer, whitelist.all)
    notifier.notifyConflicts(currentConflicts, previousConflicts)

    Ports.writeRecords(results, config.currentResultFile)
    Ports.writeRecords(mergedResults, config.currentMergedFile)
    Ports.writeConflicts(currentConflicts, config.currentConflictFile)

    Ports.writeClaims(unclaimed, config.currentUnclaimedFile)
    Ports.writeClaims(overclaimed, config.currentOverclaimedFile)

    startDate = startDate.plusDays(1)
  }

  def configureLogging(){
    val configurator = new JoranConfigurator
    val context = org.slf4j.LoggerFactory.getILoggerFactory.asInstanceOf[ch.qos.logback.classic.LoggerContext]
    try{
      configurator.setContext(context)
      context.reset()
      configurator.doConfigure(s"conf/logback-$environment.xml")
    }catch {
      case e: JoranException =>
    }
  }
}
