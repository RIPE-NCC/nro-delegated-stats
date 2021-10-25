package net.ripe.rpki.nro

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Environments.{Environments, Production}
import net.ripe.rpki.nro.Operations.{Generate, Notify, Operations}
import net.ripe.rpki.nro.main.Stats
import net.ripe.rpki.nro.model.{Conflict, Records}
import net.ripe.rpki.nro.service.{Notifier, Ports}
import scopt.OParser

import java.time.LocalDate

object Operations extends Enumeration {
  type Operations = Value
  val Generate, Notify = Value
}

object Environments extends Enumeration {
  type Environments = Value
  val Production, Prepdev, Local = Value
}

case class CommandLineOptions(
                               startDate: LocalDate = LocalDate.now(),
                               endDate: LocalDate = LocalDate.now(),
                               ownIana: Boolean = false,
                               environment: Environments = Environments.Local,
                               operation: Operations = Operations.Generate,
                             )

object CommandLineOptions {
  implicit val localDateRead: scopt.Read[LocalDate] = scopt.Read.reads(LocalDate.parse)
  implicit val operationsReads: scopt.Read[Operations.Value] = scopt.Read.reads(Operations withName _.capitalize)
  implicit val environmentReads: scopt.Read[Environments.Value] = scopt.Read.reads(Environments withName _.capitalize)
}

object Main extends Stats with App {


  val builder = OParser.builder[CommandLineOptions]
  val argsParser = {
    import net.ripe.rpki.nro.Main.builder._
    import CommandLineOptions._
    OParser.sequence(
      programName("NRO Delegated Stats"),
      head("NRO Delegated Stats 0.1"),
      opt[LocalDate]('s',"startDate")
        .action((startDateArgs, cli) => cli.copy(startDate=startDateArgs))
        .text("Start date for processing nro delegated stat: YYYY-MM-DD"),
      opt[LocalDate]('e',"endDate")
        .action((endDateArgs, cli) => cli.copy(endDate=endDateArgs))
        .text("End date for processing nro delegated stat: YYYY-MM-DD"),
      opt[Unit]("ownIana")
        .action((_, cli) ⇒ cli.copy(ownIana = true))
        .text("Use own generated IANA file as input"),
      opt[Environments]("environment")
        .action((env, cli) ⇒ cli.copy(environment = env))
        .text("Optional environment where the script runs. \nAvailable option values: local | prepdev | production"),
     opt[Operations]("operation")
        .required()
        .action((operationArgs, cli) ⇒ cli.copy(operation = operationArgs))
        .text("Operations to perform whether to generate the NRO Delegated Stats, or to perform notification/email. \nAvailable option values : generate | notify"),
    )
  }

  var CommandLineOptions(startDate, endDate, ownIana, environment, operation) =
    OParser.parse(argsParser, args, CommandLineOptions()) match {
      case Some(commandLineOptions) => commandLineOptions
      case _ => System.exit(1) // some options parse error, usage message from scopt will be shown
    }

  configureLogging()

  operation match {
    case Generate ⇒ generateDelegatedStats()
    case Notify ⇒ checkConflictsAndNotify()
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

  def checkConflictsAndNotify() : Unit = {

    Configs.config = new Configs(startDate)

    val (allowedList, previousConflicts, currentConflicts): (Records, List[Conflict], List[Conflict]) = Ports.getConflicts()
    logger.info("Allowed list:")
    allowedList.all.foreach(item ⇒ logger.info(item.toString))

    logger.info("Current conflict:")
    currentConflicts.foreach(item ⇒ logger.info(item.toString))

    logger.info("Previous conflict:")
    currentConflicts.foreach(item ⇒ logger.info(item.toString))

    val notifier = new Notifier(mailer, allowedList.all)
    logger.info(notifier.notifyConflicts(currentConflicts, previousConflicts))
  }

  def configureLogging(){
    val configurator = new JoranConfigurator
    val context = org.slf4j.LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    try{
      configurator.setContext(context)
      context.reset()
      configurator.doConfigure(s"conf/logback-$environment.xml")
    }catch {
      case e: JoranException =>
    }
  }
}
