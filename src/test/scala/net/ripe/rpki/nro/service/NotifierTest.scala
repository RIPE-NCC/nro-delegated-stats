package net.ripe.rpki.nro.service

import com.icegreen.greenmail.util.{GreenMail, ServerSetupTest}
import courier.Mailer
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Const.{APNIC, RSCG}
import net.ripe.rpki.nro.TestUtil
import org.scalatest.{BeforeAndAfter, FlatSpec}

class NotifierTest extends FlatSpec with TestUtil with BeforeAndAfter {


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

    val stickyConflicts = subject.findStickyConflicts(previousConflicts, currentConflicts)
    subject.notifyOnIssues(stickyConflicts, Set())

    val messages = greenMail.getReceivedMessages.toList

    // Sending to apnic and rscg
    assert(messages.size == 2)
    val recipients = messages.flatMap(_.getAllRecipients).map(_.toString).toSet
    assert(recipients.contains(contacts(APNIC)))
    assert(recipients.contains(contacts(RSCG)))

    // All from no-reply nro.net
    assert(messages.flatMap(_.getFrom).map(_.toString).toSet == Set(sender))

    // Same subjects
    assert(messages.map(_.getSubject).toSet == Set(s"There are problematic delegated stats since ${config.PREV_CONFLICT_DAY}"))

    allowedList.foreach { allowedListed =>
      messages.foreach(mimeMessage =>
        assert(!mimeMessage.getContent().toString.contains(allowedListed))
      )
    }
  }

  it should " notify relevant RIR contacts and RSCG coordinator if there is conflict and/or currentUnclaimed space" in {
    val previousConflicts = Ports.parseConflicts(getResourceReader("/previousConflicts"))
    val currentConflicts = Ports.parseConflicts(getResourceReader("/currentConflicts"))
    val previousUnclaimed = Ports.parseUnclaimed(getResourceReader("/previousUnclaimed"))
    val currentUnclaimed = Ports.parseUnclaimed(getResourceReader("/currentUnclaimed"))

    val stickyConflicts = subject.findStickyConflicts(previousConflicts, currentConflicts)
    val stickyUnclaimed = subject.findStickyUnclaimed(previousUnclaimed, currentUnclaimed)
    subject.notifyOnIssues(stickyConflicts, stickyUnclaimed)

    val messages = greenMail.getReceivedMessages.toList

    // Sending to apnic and rscg
    assert(messages.size == 2)
    val recipients = messages.flatMap(_.getAllRecipients).map(_.toString).toSet
    assert(recipients.contains(contacts(APNIC)))
    assert(recipients.contains(contacts(RSCG)))

    // All from no-reply nro.net
    assert(messages.flatMap(_.getFrom).map(_.toString).toSet == Set(sender))

    // Same subjects
    assert(messages.map(_.getSubject).toSet == Set(s"There are problematic delegated stats since ${config.PREV_CONFLICT_DAY}"))

    allowedList.foreach { allowedListed =>
      messages.foreach { mimeMessage =>
        val content = mimeMessage.getContent().toString
        assert(!content.contains(allowedListed))
        assert(content.contains("Please verify the following unclaimed resources"))
        assert(content.contains("apnic|ZZ|ipv4|200.0.113.0|256|20190920|reserved||e-stats"))
        assert(content.contains("Please verify the following resource conflicts"))
        assert(content.contains("afrinic|ZZ|ipv6|2001:43f8:d8::|45|20250923|reserved|afrinic|e-stats"))
      }
    }
  }

  it should " be quiet if conflict disappears" in {
    val previousConflicts = Ports.parseConflicts(getResourceReader("/previousConflicts"))
    val currentConflicts = List()
    val stickyConflicts = subject.findStickyConflicts(previousConflicts, currentConflicts)
    assert(stickyConflicts.isEmpty)
  }

  it should " not alert if there are just new conflicts" in {
    val previousConflicts = List()
    val currentConflicts = Ports.parseConflicts(getResourceReader("/currentConflicts"))
    val stickyConflicts = subject.findStickyConflicts(previousConflicts, currentConflicts)
    assert(stickyConflicts.isEmpty)
  }

  it should " able to detect allowedlisted conflict " in {
    val conflicts = Ports.parseConflicts(getResourceReader("/previousConflicts"))

    val (allowed, notAllowed) = conflicts.partition(subject.isAllowed)

    allowed.foreach(c => assert(allowedList.contains(c.a) || allowedList.contains(c.b)))
    notAllowed.foreach(c => assert(!allowedList.contains(c.a) && !allowedList.contains(c.b)))
  }
}
