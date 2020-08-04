package net.ripe.rpki.nro.service

import javax.mail.Provider
import javax.mail.internet.MimeMessage
import net.ripe.rpki.nro.TestUtil
import org.jvnet.mock_javamail._
import org.scalatest.FlatSpec
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Const.{APNIC, RSCG}

class MockedSMTPProvider extends Provider(Provider.Type.TRANSPORT, "mocked", classOf[MockTransport].getName, "Mock", null)

class NotifierTest extends FlatSpec with TestUtil {

  val allowedList = Ports.parseRecordSource("allowedlist").all
  val subject = new Notifier(mockMailer, allowedList)

  "Notifier test " should " notify relevant RIR contacts and RSCG coordinator if there is conflict" in {
    val previousConflicts = Ports.readConflicts(getResourceFile("/previousConflicts"))
    val currentConflicts = Ports.readConflicts(getResourceFile("/currentConflicts"))

    subject.notifyConflicts(currentConflicts, previousConflicts)


    val apnic = Mailbox.get(contacts(APNIC))
    assert(apnic.size === 1)
    val mimeMessage = apnic.get(0).asInstanceOf[MimeMessage]
    assert(mimeMessage.getFrom.head.toString === sender)
    assert(mimeMessage.getSubject === s"There are conflicting delegated stats since ${config.PREV_CONFLICT_DAY}")

    allowedList.foreach{allowedListed => 
        assert(!mimeMessage.getContent().toString.contains(allowedListed))
    }    

    val rscg = Mailbox.get(contacts(RSCG))
    assert(rscg.size === 1)
    val mimeMessageRscg = rscg.get(0).asInstanceOf[MimeMessage]
    assert(mimeMessageRscg.getFrom.head.toString === sender)
    assert(mimeMessageRscg.getSubject === s"There are conflicting delegated stats since ${config.PREV_CONFLICT_DAY}")

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

  it should " able to detect allowedlisted conflict " in {
    val conflicts = Ports.readConflicts(getResourceFile("/previousConflicts"))

    val (allowed, notAllowed) = conflicts.partition(subject.isAllowed)
  
    allowed.foreach(c => assert(allowedList.contains(c.a) || allowedList.contains(c.b)))
    notAllowed.foreach(c => assert(!allowedList.contains(c.a) && !allowedList.contains(c.b)))
  
  }

}
