package net.ripe.rpki.nro

import org.slf4j.LoggerFactory

object Alert {
  val logger = LoggerFactory.getLogger(Alert.getClass)

  def alertConflicts(current: List[Conflict], previous: List[Conflict]): Set[Conflict] = {

    val currentMap = current.map(c => (c.key -> c)).toMap
    val previousMap = previous.map(c => (c.key -> c)).toMap

    val persistingConflicts = currentMap.keySet.intersect(previousMap.keySet).map(previousMap)

    if(persistingConflicts.nonEmpty){
      logger.info("Found persisting conflicts")
      persistingConflicts.foreach(c => logger.info(c.toString))
    }

    persistingConflicts
  }
}
