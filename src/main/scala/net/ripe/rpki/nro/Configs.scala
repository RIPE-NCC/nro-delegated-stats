package net.ripe.rpki.nro

import java.io.File
import java.time.LocalDate

import com.typesafe.config.{Config, ConfigFactory}

object Configs {

  val config: Config = ConfigFactory.load()

  val gracePeriod: Int = config.getInt("grace.period")
  val dataDirectory: String = config.getString("data.directory")
  val resultDirectory: String = config.getString("result.directory")
  val resultFileName: String = config.getString("result.fileName")
  val conflictFileName: String = config.getString("conflict.fileName")

  val TODAY: String = formatDate(java.time.LocalDate.now)
  val PREV_RESULT_DAY: String = formatDate(java.time.LocalDate.now.minusDays(1))
  val PREV_CONFLICT_DAY: String = formatDate(java.time.LocalDate.now.minusDays(gracePeriod))

  val currentResultFile: String = s"$resultDirectory/$TODAY/$resultFileName"
  val previousResultFile: String = s"$resultDirectory/$PREV_RESULT_DAY/$resultFileName"
  val currentConflictFile: String = s"$resultDirectory/$TODAY/$conflictFileName"
  val previousConflictFile: String = s"$resultDirectory/$PREV_CONFLICT_DAY/$conflictFileName"

  val todayDir: File = {
    val resultFile = new File(s"$resultDirectory/$TODAY")
    if(!resultFile.exists()){
      resultFile.mkdir()
    }

    resultFile
  }

  def formatDate(date: LocalDate): String = date.toString.replaceAll("-", "")
}
