package net.akouryy.kreins
package game

import org.scalatest._

class PosSpec extends FlatSpec with DiagrammedAssertions {
  "rotate180" should "180度回転する" in {
    Seq(
      63, 62, 61, 60, 59, 58, 57, 56,
      55, 54, 53, 52, 51, 50, 49, 48,
      47, 46, 45, 44, 43, 42, 41, 40,
      39, 38, 37, 36, 35, 34, 33, 32,
      31, 30, 29, 28, 27, 26, 25, 24,
      23, 22, 21, 20, 19, 18, 17, 16,
      15, 14, 13, 12, 11, 10, 9, 8,
      7, 6, 5, 4, 3, 2, 1, 0
    ).zipWithIndex.foreach { case (p, r) =>
      assert(Pos.rotate180(p.toByte) === r.toByte)
    }
  }

  "mirrorWithDiagRightDown" should "左上-右下軸で折り返す" in {
    Seq(
      0, 8, 16, 24, 32, 40, 48, 56,
      1, 9, 17, 25, 33, 41, 49, 57,
      2, 10, 18, 26, 34, 42, 50, 58,
      3, 11, 19, 27, 35, 43, 51, 59,
      4, 12, 20, 28, 36, 44, 52, 60,
      5, 13, 21, 29, 37, 45, 53, 61,
      6, 14, 22, 30, 38, 46, 54, 62,
      7, 15, 23, 31, 39, 47, 55, 63
    ).zipWithIndex.foreach { case (p, r) =>
      assert(Pos.mirrorWithDiagRightDown(p.toByte) === r.toByte)
    }
  }

  "mirrorWithDiagRightUp" should "左下-右上軸で折り返す" in {
    Seq(
      63, 55, 47, 39, 31, 23, 15, 7,
      62, 54, 46, 38, 30, 22, 14, 6,
      61, 53, 45, 37, 29, 21, 13, 5,
      60, 52, 44, 36, 28, 20, 12, 4,
      59, 51, 43, 35, 27, 19, 11, 3,
      58, 50, 42, 34, 26, 18, 10, 2,
      57, 49, 41, 33, 25, 17, 9, 1,
      56, 48, 40, 32, 24, 16, 8, 0
    ).zipWithIndex.foreach { case (p, r) =>
      assert(Pos.mirrorWithDiagRightUp(p.toByte) === r.toByte)
    }
  }
}
