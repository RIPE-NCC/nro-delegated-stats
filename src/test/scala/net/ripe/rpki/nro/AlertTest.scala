package net.ripe.rpki.nro

import org.scalatest.FlatSpec

class AlertTest extends FlatSpec with TestUtil {

  "Alert test " should " let me know if there are persistent conflicts" in {
      val previousConflicts = Ports.readConflicts(getResourceFile("/previousConflicts"))
      val currentConflicts = Ports.readConflicts(getResourceFile("/currentConflicts"))
      val alertSent = Alert.alertConflicts(currentConflicts, previousConflicts)

    assert(alertSent.size == 3)

  }
}
