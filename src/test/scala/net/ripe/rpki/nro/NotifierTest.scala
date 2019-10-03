package net.ripe.rpki.nro

import javax.mail.Provider
import javax.mail.internet.MimeMessage
import org.jvnet.mock_javamail._
import org.scalatest.FlatSpec
import Settings._

class MockedSMTPProvider extends Provider(Provider.Type.TRANSPORT, "mocked", classOf[MockTransport].getName, "Mock", null)

class NotifierTest extends FlatSpec with TestUtil {

  val subject = new Notifier(mockMailer)

  "Notifier test " should " let me know if there are persistent conflicts" in {
    val previousConflicts = Ports.readConflicts(getResourceFile("/previousConflicts"))
    val currentConflicts = Ports.readConflicts(getResourceFile("/currentConflicts"))

    subject.notifyConflicts(currentConflicts, previousConflicts)


    val inbox = Mailbox.get(Settings.contacts("apnic"))
    assert(inbox.size === 1)
    val mimeMessage = inbox.get(0).asInstanceOf[MimeMessage]
    assert(mimeMessage.getFrom.head.toString === sender)
    assert(mimeMessage.getSubject === s"There are conflicting delegated stats since ${Settings.PREV_CONFLICT_DAY}")

  }

  it should " be quiet if conflict disappears" in {
    val previousConflicts = Ports.readConflicts(getResourceFile("/previousConflicts"))
    val currentConflicts = List()
    val alertSent = subject.notifyConflicts(currentConflicts, previousConflicts)

    assert(alertSent == "No conflicts, no mail")
  }

  it should " not alert if there are just new conflicts" in {
    val previousConflicts = List()
    val currentConflicts = Ports.readConflicts(getResourceFile("/currentConflicts"))
    val alertSent = subject.notifyConflicts(currentConflicts, previousConflicts)

    assert(alertSent == "No conflicts, no mail")
  }


}
