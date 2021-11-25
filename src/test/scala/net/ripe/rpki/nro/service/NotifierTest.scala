package net.ripe.rpki.nro.service

import com.icegreen.greenmail.util.{GreenMail, ServerSetupTest}
import courier.Mailer
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Const.{APNIC, RSCG}
import net.ripe.rpki.nro.TestUtil
import org.junit.AfterClass
import org.scalatest.{BeforeAndAfter, FlatSpec}

import java.io.FileReader

class NotifierTest extends FlatSpec with TestUtil with BeforeAndAfter{


  val greenMail = new GreenMail(ServerSetupTest.ALL)

  val mockedSession = greenMail.getSmtp().createSession()
  val mockMailer = Mailer(mockedSession)
  val allowedList = Ports.parseRecordSource(scala.io.Source.fromResource("allowedlist")).all
  val subject = new Notifier(mockMailer, allowedList)

  before {
      greenMail.start()
  }

  after {
    greenMail.stop()
  }

  "Notifier test " should " notify relevant RIR contacts and RSCG coordinator if there is conflict" in {
    val previousConflicts = Ports.parseConflicts(getResourceReader("/previousConflicts"))
    val currentConflicts = Ports.parseConflicts(getResourceReader("/currentConflicts"))

    val stickyConflicts = subject.findStickyConflicts(currentConflicts, previousConflicts)
    subject.notifyConflicts(stickyConflicts)

    val messages = greenMail.getReceivedMessages.toList

    // Sending to apnic and rscg
    assert(messages.size == 2)
    val recipients = messages.flatMap(_.getAllRecipients).map(_.toString).toSet
    assert(recipients.contains(contacts(APNIC)))
    assert(recipients.contains(contacts(RSCG)))

    // All from no-reply nro.net
    assert(messages.flatMap(_.getFrom).map(_.toString).toSet == Set("no-reply@nro.net"))

    // Same subjects
    assert(messages.map(_.getSubject).toSet == Set(s"There are conflicting delegated stats since ${config.PREV_CONFLICT_DAY}"))

    allowedList.foreach{allowedListed =>
      messages.foreach(mimeMessage =>
        assert(!mimeMessage.getContent().toString.contains(allowedListed))
      )
    }
  }

  it should " be quiet if conflict disappears" in {
    val previousConflicts = Ports.parseConflicts(getResourceReader("/previousConflicts"))
    val currentConflicts = List()
    val stickyConflicts = subject.findStickyConflicts(currentConflicts, previousConflicts)

    subject.notifyConflicts(stickyConflicts)

    val messages = greenMail.getReceivedMessages.toList
    assert(messages.isEmpty)
  }

  it should " not alert if there are just new conflicts" in {
    val previousConflicts = List()
    val currentConflicts = Ports.parseConflicts(getResourceReader("/currentConflicts"))
    val stickyConflicts = subject.findStickyConflicts(currentConflicts, previousConflicts)
    val alertSent = subject.notifyConflicts(stickyConflicts)

    val messages = greenMail.getReceivedMessages.toList
    assert(messages.isEmpty)
  }

  it should " able to detect allowedlisted conflict " in {
    val conflicts = Ports.parseConflicts(getResourceReader("/previousConflicts"))

    val (allowed, notAllowed) = conflicts.partition(subject.isAllowed)

    allowed.foreach(c => assert(allowedList.contains(c.a) || allowedList.contains(c.b)))
    notAllowed.foreach(c => assert(!allowedList.contains(c.a) && !allowedList.contains(c.b)))
  }
}
