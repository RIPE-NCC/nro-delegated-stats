package net.ripe.rpki.nro

import java.io.StringReader
import java.util.Properties

import com.github.tototoshi.csv.CSVReader
import courier.Mailer
import net.ripe.rpki.nro.model.Record
import net.ripe.rpki.nro.service.MockedSMTPProvider
import net.ripe.rpki.nro.service.Ports.PipeFormat

trait TestUtil {

  def toRecords(testInputs: String): List[Record] =
    CSVReader.open(new StringReader(testInputs)).all.map(Record.apply)

  def getResourceFile(fileName: String): String = getClass.getResource(fileName).getFile

  lazy val mockMailer: Mailer = {
    val mockedProperty = new Properties()
    mockedProperty.put("mail.transport.protocol.rfc822", "mocked")

    val mockedSession = javax.mail.Session.getDefaultInstance(mockedProperty)
    mockedSession.setProvider(new MockedSMTPProvider)

    Mailer(mockedSession)
  }
}
