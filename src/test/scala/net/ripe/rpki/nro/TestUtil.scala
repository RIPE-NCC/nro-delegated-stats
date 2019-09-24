package net.ripe.rpki.nro

import java.io.StringReader

import com.github.tototoshi.csv.CSVReader

import Ports.PipeFormat

trait TestUtil {

  def toRecords(testInputs: String): List[Record] =
    CSVReader.open(new StringReader(testInputs)).all.map(Record.apply)

  def getResourceFile(fileName: String): String = getClass.getResource(fileName).getFile

}
