package net.ripe.rpki.nro

import java.math.BigInteger

object Util {

  val bigTwo = new BigInteger("2")

  def validIpv6(start: BigInteger, length: Integer): Boolean = {
    val modulo = bigTwo.pow(128 - length)
    start.mod(modulo) == BigInteger.ZERO
  }


}
