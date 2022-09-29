package net.ripe.rpki.nro

import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.iana.IanaGenerator
import net.ripe.rpki.nro.main.Stats
import net.ripe.rpki.nro.model.{Conflict, Records}
import net.ripe.rpki.nro.service.{Notifier, Ports}
import org.slf4j.LoggerFactory
import scopt.OParser

import java.net.URL
import java.time.LocalDate
import scala.util.{Failure, Success, Try}

case class CommandLineOptions(
                               operation: String = "",
                               startDate: LocalDate = LocalDate.now(),
                               endDate: LocalDate = LocalDate.now(),
                               ownIana: Boolean = false,
                               base: String = "https://ftp.ripe.net/pub/stats/ripencc/nro-stats",
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
      opt[String]('b', "base-url")
        .text("Base URL for retrieving previously generated files. Defaults to https://ftp.ripe.net/pub/stats/ripencc/nro-stats.")
        .action((url, cli) => cli.copy(base = if (url.last == '/') url.dropRight(1) else url)),
      help('h', "help").text("print this message"),
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
            .text("Use own generated IANA file as input, defaults to using https://ftp.apnic.net/stats/iana/delegated-iana-latest"),
        ),
      cmd("notify")
        .text("Notify RS contacts if there are persistent conflicts over a grace period")
        .action((_, cli) => cli.copy(operation = "notify"))
        .children(
          opt[LocalDate]('c', "conflict-date")
            .text("Current conflict date, defaults to today: YYYY-MM-DD")
            .action((conflictDateArgs, cli) => cli.copy(conflictDate = conflictDateArgs))
        ),
      cmd("iana-file")
        .text("Generate IANA file based on  numbers and resources from iana.org ")
        .action((_, cli) => cli.copy(operation = "iana-file")
        ),
      checkConfig(cli => if (cli.operation.isEmpty) failure("You need to specify operations [generate | notify] ") else success),
      checkConfig(cli => Try(new URL(cli.base).getHost) match {
        case Success(_)  => success
        case Failure(e) => failure(s"Base URL can't be parsed ${e.getMessage}")
      })
    )
  }

  var CommandLineOptions(operation, startDate, endDate, ownIana, baseURL, conflictDate) =
    OParser.parse(argsParser, args, CommandLineOptions()) match {
      case Some(commandLineOptions) => commandLineOptions
      case _ => System.exit(1) // some options parse error, usage message from scopt will be shown
    }


  operation match {
    case "generate" => generateDelegatedStats(baseURL)
    case "notify"   => checkConflictsAndNotify(baseURL)
    case "iana-file" => generateIanaFile()
  }

  def generateDelegatedStats(baseURL: String): Unit = {

    if (startDate.equals(endDate)) {
      logger.info(s"Generating stats for a single day $startDate")
    } else {
      logger.info(s"Generating stats from $startDate to $endDate")
    }

    while (startDate.compareTo(endDate) <= 0) {

      Configs.configureFor(startDate)
      logger.info(s"Data dir: ${Configs.config.currentDataDirectory}")
      logger.info(s"Result dir: ${Configs.config.currentResultDirectory}")

      val (rirRecords, ianaRecord, previousResult) = Ports.fetchAndParseInputs(baseURL, ownIana)
      val (results, mergedResults, currentConflicts, unclaimed, overclaimed) = process(rirRecords, ianaRecord, previousResult)


      Ports.writeRecords(results, config.currentResultFile)
      Ports.writeRecords(mergedResults, config.currentMergedFile)
      Ports.writeConflicts(currentConflicts, config.currentConflictFile)

      Ports.writeClaims(unclaimed, config.currentUnclaimedFile)
      Ports.writeClaims(overclaimed, config.currentOverclaimedFile)

      startDate = startDate.plusDays(1)
    }
  }

  def generateIanaFile(): Unit = {
    val ianaRecords = IanaGenerator.processIanaRecords
    Ports.writeRecords(ianaRecords, config.currentIanaFile, disclaimer=true)
  }

  def checkConflictsAndNotify(baseConflictsURL: String) : Unit = {

    Configs.configureFor(conflictDate)
    // Here we already have to modify how we are retrieving conflicts.
    val (allowedList, previousConflicts, currentConflicts): (Records, Seq[Conflict], Seq[Conflict]) = Ports.getConflicts(baseConflictsURL)

    val notifier = new Notifier(mailer, allowedList.all)
    val stickyConflicts = notifier.findStickyConflicts(currentConflicts, previousConflicts)
    if (stickyConflicts.isEmpty) {
      logger.info("No emails sent.")
    } else {
      logger.info("Found persistent conflicts:")
      stickyConflicts.foreach(c => logger.info(c.toString))
      notifier.notifyConflicts(stickyConflicts)
    }
  }

}
