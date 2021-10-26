package net.ripe.rpki.nro

import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.main.Stats
import net.ripe.rpki.nro.model.{Conflict, Records}
import net.ripe.rpki.nro.service.{Notifier, Ports}
import org.slf4j.LoggerFactory
import scopt.OParser

import java.time.LocalDate

case class CommandLineOptions(
                               operation: String = "",
                               startDate: LocalDate = LocalDate.now(),
                               endDate: LocalDate = LocalDate.now(),
                               ownIana: Boolean = false,
                               base: String = "https://ftp.ripe.net/pub/stats/ripencc/nro-stats/",
                               conflictDate: LocalDate = LocalDate.now(),
                             )

object CommandLineOptions {
  implicit val localDateRead: scopt.Read[LocalDate] = scopt.Read.reads(LocalDate.parse)
}

object Main extends Stats with App {

  override val logger = LoggerFactory.getLogger(Main.getClass)

  val builder = OParser.builder[CommandLineOptions]
  val argsParser = {
    import net.ripe.rpki.nro.CommandLineOptions._
    import net.ripe.rpki.nro.Main.builder._
    OParser.sequence(
      head("NRO Extended Allocation and Assignments Statistics"),
      programName("java -jar nro-delegated-stats.jar"),
      cmd("generate")
        .text("Generate NRO Delegated Extended Statistic, based on each RIRs delegated stats and IANA file")
        .action((_, cli) => cli.copy(operation = "generate"))
        .children(
          opt[LocalDate]('s', "startDate")
            .action((startDateArgs, cli) => cli.copy(startDate = startDateArgs))
            .text("Start date for processing NRO delegated stat, default to today: YYYY-MM-DD"),
          opt[LocalDate]('e', "endDate")
            .action((endDateArgs, cli) => cli.copy(endDate = endDateArgs))
            .text("End date for processing NRO delegated stat, default to today: YYYY-MM-DD"),
          opt[Unit]("ownIana")
            .action((_, cli) => cli.copy(ownIana = true))
            .text("Use own generated IANA file as input"),
        ),
      cmd("notify")
        .text("Notify RS contacts if there are persistent conflicts over a grace period")
        .action((_, cli) => cli.copy(operation = "notify"))
        .children(
          opt[String]('b', "base-url")
            .text("Base url for retrieving conflicts, defaults to: https://ftp.ripe.net/pub/stats/ripencc/nro-stats/.")
            .action((baseArgs, cli) => cli.copy(base = baseArgs)),
          opt[LocalDate]('c', "conflict-date")
            .text("Current conflict date, defaults to today: YYYY-MM-DD")
            .action((conflictDateArgs, cli) => cli.copy(conflictDate = conflictDateArgs))
        ),
      checkConfig(cli => if (cli.operation.isEmpty) failure("You need to specify operations [generate | notify] ") else success)
    )
  }

  var CommandLineOptions(operation, startDate, endDate, ownIana, baseConflictsURL, conflictDate) =
    OParser.parse(argsParser, args, CommandLineOptions()) match {
      case Some(commandLineOptions) => commandLineOptions
      case _ => System.exit(1) // some options parse error, usage message from scopt will be shown
    }

  operation match {
    case "generate" => generateDelegatedStats()
    case "notify"   => checkConflictsAndNotify(baseConflictsURL)
  }

  def generateDelegatedStats(): Unit = {

    if (startDate.equals(endDate)) {
      logger.info(s"Generating stats for a single day $startDate")
    } else {
      logger.info(s"Generating stats from $startDate to $endDate")
    }

    while (startDate.compareTo(endDate) <= 0) {

      Configs.configureFor(startDate)
      logger.info(s"Data dir: $Configs.config.currentDataDirectory")
      logger.info(s"Result dir: $Configs.config.currentResultDirectory")

      val (rirRecords, ianaRecord, previousResult) = Ports.fetchAndParseInputs(ownIana)
      val (results, mergedResults, currentConflicts, unclaimed, overclaimed) = process(rirRecords, ianaRecord, previousResult)


      Ports.writeRecords(results, config.currentResultFile)
      Ports.writeRecords(mergedResults, config.currentMergedFile)
      Ports.writeConflicts(currentConflicts, config.currentConflictFile)

      Ports.writeClaims(unclaimed, config.currentUnclaimedFile)
      Ports.writeClaims(overclaimed, config.currentOverclaimedFile)

      startDate = startDate.plusDays(1)
    }
  }

  def checkConflictsAndNotify(baseConflictsURL: String) : Unit = {

    Configs.configureFor(conflictDate)
    // Here we already have to modify how we are retrieving conflicts.
    val (allowedList, previousConflicts, currentConflicts): (Records, List[Conflict], List[Conflict]) = Ports.getConflicts(baseConflictsURL)
    logger.info("Allowed list:")
    allowedList.all.foreach(item => logger.info(item.toString))

    logger.info("Current conflict:")
    currentConflicts.foreach(item => logger.info(item.toString))

    logger.info("Previous conflict:")
    currentConflicts.foreach(item => logger.info(item.toString))

    val notifier = new Notifier(mailer, allowedList.all)
    val stickyConflicts = notifier.findStickyConflicts(currentConflicts, previousConflicts)
    notifier.notifyConflicts(stickyConflicts)
  }

}
