package net.akouryy.kreins
package util

import org.scalatest._
import scala.util.Random

class BitUtilSpec extends FlatSpec with DiagrammedAssertions {
  "popcount" should "立っているビット数を返す" in {
    assert(BitUtil.popcount(0) === 0)

    for(i <- 0 to 63) {
      assert(BitUtil.popcount((1L << i) - 1) === i)
      assert(BitUtil.popcount(-(1L << i)) === 64 - i)
    }
  }

  "firstHighBit" should "一番上の立っているビット以外を下ろす" in {
    assert(BitUtil.firstHighBit(0) === 0)

    for(i <- 0 to 63) {
      for(_ <- 0 until 10) {
        assert(BitUtil.firstHighBit(1L << i | (Random.nextLong() & (1L << i) - 1L))
          === (1L << i))
      }
    }
  }

  "firstHighBitPos" should "一番上の立っているビット位置を返す" in {
    assert(BitUtil.firstHighBitPos(0) === -1)

    for(i <- 0 to 63) {
      for(_ <- 0 until 10) {
        assert(BitUtil.firstHighBitPos(1L << i | (Random.nextLong() & (1L << i) - 1L))
          === i)
      }
    }
  }

  "lastHighBitPos" should "一番下の立っているビット位置を返す" in {
    assert(BitUtil.lastHighBitPos(0) === 64)

    for(i <- 0 to 63) {
      for(_ <- 0 until 10) {
        val r = (1L << i) | Random.nextLong() << i
        assert(BitUtil.lastHighBitPos(r) === i)
      }
    }
  }
}
