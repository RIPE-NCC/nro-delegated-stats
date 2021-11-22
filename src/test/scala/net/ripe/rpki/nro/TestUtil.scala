package net.ripe.rpki.nro

import java.io.{FileReader, Reader, StringReader}
import java.util.Properties
import com.github.tototoshi.csv.CSVReader
import courier.Mailer
import net.ripe.rpki.nro.model.Record
import net.ripe.rpki.nro.service.Ports.PipeFormat

trait TestUtil {


  def toRecords(testInputs: String): Seq[Record] =
    CSVReader.open(new StringReader(testInputs)).toStream.map(Record.apply).toSeq

  def getResourceFile(fileName: String): String = getClass.getResource(fileName).getFile

  def getResourceReader(fileName:String): Reader = new FileReader(getResourceFile(fileName))

}
