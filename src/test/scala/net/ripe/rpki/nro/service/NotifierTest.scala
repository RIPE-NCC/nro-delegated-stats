package net.ripe.rpki.nro.service

import com.icegreen.greenmail.util.{GreenMail, ServerSetupTest}
import courier.Mailer
import net.ripe.rpki.nro.Configs._
import net.ripe.rpki.nro.Const._
import net.ripe.rpki.nro.TestUtil
import org.scalatest.{BeforeAndAfter, FlatSpec}

class NotifierTest extends FlatSpec with TestUtil with BeforeAndAfter {

  val mockMailer = Mailer(new GreenMail(ServerSetupTest.ALL).getSmtp.createSession())
  val allowedList = Ports.parseRecordSource(scala.io.Source.fromResource("allowedlist")).all
  val subject = new Notifier(mockMailer, allowedList)

  "Notifier test " should " notify relevant RIR contacts and RSCG coordinator if there is conflict" in {
    val stickyConflicts = subject.findStickyConflicts(
      Ports.parseConflicts(getResourceReader("/previousConflicts")),
      Ports.parseConflicts(getResourceReader("/currentConflicts")))

    val envelopes = subject.createEnvelopes(stickyConflicts, Set())

    assert(envelopes(Notifier.CONFLICTS).subject.get._1 == s"There are conflicting delegated stats since ${config.PREV_CONFLICT_DAY}")
    assert(envelopes(Notifier.CONFLICTS).to.map(_.toString).toSet == Set(APNIC).map(contacts))
    assert(envelopes(Notifier.CONFLICTS).from.toString == sender)
    assert(envelopes(Notifier.BOTH_CONFLICTS_AND_UNCLAIMED).subject.get._1 == s"There are problematic delegated stats since ${config.PREV_CONFLICT_DAY}")
    assert(envelopes(Notifier.BOTH_CONFLICTS_AND_UNCLAIMED).to.map(_.toString).toSet == Set(RSCG).map(contacts))
    assert(envelopes(Notifier.BOTH_CONFLICTS_AND_UNCLAIMED).from.toString == sender)

    allowedList.foreach { allowedListed =>
      assert(!envelopes(Notifier.CONFLICTS).contents.toString.contains(allowedListed))
      assert(!envelopes(Notifier.BOTH_CONFLICTS_AND_UNCLAIMED).contents.toString.contains(allowedListed))
    }
  }

  it should " notify relevant RIR contacts and RSCG coordinator if there is conflict and/or currentUnclaimed space" in {
    val previousConflicts = Ports.parseConflicts(getResourceReader("/previousConflicts"))
    val currentConflicts = Ports.parseConflicts(getResourceReader("/currentConflicts"))
    val previousUnclaimed = Ports.parseUnclaimed(getResourceReader("/previousUnclaimed"))
    val currentUnclaimed = Ports.parseUnclaimed(getResourceReader("/currentUnclaimed"))

    val stickyConflicts = subject.findStickyConflicts(previousConflicts, currentConflicts)
    val stickyUnclaimed = subject.findStickyUnclaimed(previousUnclaimed, currentUnclaimed)

    val envelopes = subject.createEnvelopes(stickyConflicts, stickyUnclaimed)

    assert(envelopes(Notifier.CONFLICTS).subject.get._1 == s"There are conflicting delegated stats since ${config.PREV_CONFLICT_DAY}")
    assert(envelopes(Notifier.CONFLICTS).from.toString == sender)
    assert(envelopes(Notifier.CONFLICTS).to.map(_.toString).toSet == Set(APNIC).map(contacts))

    assert(envelopes(Notifier.BOTH_CONFLICTS_AND_UNCLAIMED).subject.get._1 == s"There are problematic delegated stats since ${config.PREV_CONFLICT_DAY}")
    assert(envelopes(Notifier.BOTH_CONFLICTS_AND_UNCLAIMED).from.toString == sender)
    assert(envelopes(Notifier.BOTH_CONFLICTS_AND_UNCLAIMED).to.map(_.toString).toSet == Set(RSCG).map(contacts))

    assert(envelopes(Notifier.UNCLAIMED).subject.get._1 == s"There are unclaimed delegated stats since ${config.PREV_CONFLICT_DAY}")
    assert(envelopes(Notifier.UNCLAIMED).from.toString == sender)
    assert(envelopes(Notifier.UNCLAIMED).to.map(_.toString).toSet == Set(AFRINIC, ARIN, RIPENCC).map(contacts))

    allowedList.foreach { allowedListed =>
      val contentConflicts = envelopes(Notifier.CONFLICTS).contents.toString
      assert(!contentConflicts.contains(allowedListed))
      assert(contentConflicts.contains("apnic|ZZ|ipv4|200.0.113.0|256|20190920|reserved||e-stats"))
      assert(contentConflicts.contains("Please verify the following resource conflicts"))

      val contentUnclaimed = envelopes(Notifier.UNCLAIMED).contents.toString
      assert(!contentUnclaimed.contains(allowedListed))
      assert(contentUnclaimed.contains("Please verify the following unclaimed resources"))
      assert(contentUnclaimed.contains("afrinic|ZZ|ipv6|2001:43f8:d8::|45|20250923|reserved|afrinic|e-stats"))

      val contentBoth = envelopes(Notifier.BOTH_CONFLICTS_AND_UNCLAIMED).contents.toString
      assert(!contentBoth.contains(allowedListed))
      assert(contentBoth.contains("Please verify the following unclaimed resources"))
      assert(contentBoth.contains("apnic|ZZ|ipv4|200.0.113.0|256|20190920|reserved||e-stats"))
      assert(contentBoth.contains("Please verify the following resource conflicts"))
      assert(contentBoth.contains("afrinic|ZZ|ipv6|2001:43f8:d8::|45|20250923|reserved|afrinic|e-stats"))
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
