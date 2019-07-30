package net.akouryy.kreins
package scorer

import org.scalatest._

class CellScorerSpec extends FlatSpec with DiagrammedAssertions {
  "panelScore" should "各セルについてその評価値を返す" in {
    Seq(
      // @formatter:off
       45, -46,   3,  -1,  -1,   3, -46,  45,
      -46, -56,  -1,  -3,  -3,  -1, -56, -46,
        3,  -1,   2,  -1,  -1,   2,  -1,   3,
       -1,  -3,  -1,   0,   0,  -1,  -3,  -1,
       -1,  -3,  -1,   0,   0,  -1,  -3,  -1,
        3,  -1,   2,  -1,  -1,   2,  -1,   3,
      -46, -56,  -1,  -3,  -3,  -1, -56, -46,
       45, -46,   3,  -1,  -1,   3, -46,  45
      // @formatter:on
    ).zipWithIndex.foreach { case (s, i) =>
      assert(CellScorer.panelScore(1L << i) === s)
    }
  }
}
