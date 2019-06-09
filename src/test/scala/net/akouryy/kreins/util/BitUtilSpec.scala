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

  /*
  "possPuttableToRight" should "石を置いて右側を返せる場所を返す" in {
    testBoard(BitUtil.possPuttableToRight, (am, bm, i, j) =>
      !am(i)(j) && !bm(i)(j) && (j + 2 to 7).exists {
        k =>
          am(i)(k) && (j + 1 until k).forall(l => bm(i)(l))
      }
    )
  }

  "possPuttable" should "石を置ける場所を返す" in {
    testBoard(BitUtil.possPuttable, (am, bm, i, j) =>
      !am(i)(j) && !bm(i)(j) &&
        (
          (j + 2 to 7).exists {
            k =>
              am(i)(k) && (j + 1 until k).forall(l => bm(i)(l))
          } ||
            (0 to j - 2).exists {
              k =>
                am(i)(k) && (k + 1 until j).forall(l => bm(i)(l))
            } ||
            (i + 2 to 7).exists {
              h =>
                am(h)(j) && (i + 1 until h).forall(g => bm(g)(j))
            } ||
            (0 to i - 2).exists {
              h =>
                am(h)(j) && (h + 1 until i).forall(g => bm(g)(j))
            } ||
            (2 to Math.min(7 - i, 7 - j)).exists {
              p =>
                am(i + p)(j + p) && (1 until p).forall(q => bm(i + q)(j + q))
            } ||
            (Math.max(-i, -j) to -2).exists {
              p =>
                am(i + p)(j + p) && (p + 1 to -1).forall(q => bm(i + q)(j + q))
            } ||
            (2 to Math.min(7 - i, j)).exists {
              p =>
                am(i + p)(j - p) && (1 until p).forall(q => bm(i + q)(j - q))
            } ||
            (Math.max(-i, j - 7) to -2).exists {
              p =>
                am(i + p)(j - p) && (p + 1 to -1).forall(q => bm(i + q)(j - q))
            }
          )
    )
  }

   */
}
