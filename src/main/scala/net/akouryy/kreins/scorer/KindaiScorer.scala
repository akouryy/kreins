package net.akouryy.kreins
package scorer

import game.Board
import util.InputUtil

/**
  * https://www.info.kindai.ac.jp/~takasi-i/thesis/2012_09-1-037-0133_S_Shiota_thesis.pdf
  */
final class KindaiScorer(val wbp: Int, val wcn: Int, val wfs: Int) extends Scorer {
  def score(board: Board) = {
    CellScorer.score(board.unfixedBoard) * wbp +
      (board.possPlaceable.popcount - board.pass.possPlaceable.popcount) * wcn +
      (board.fst.fixedRectangleCount - board.snd.fixedRectangleCount) * wfs
  }
}

object KindaiScorer {
  def fromStdin() = {
    val wbp = InputUtil.readInt("wbp(3): ").getOrElse(3)
    val wcn = InputUtil.readInt("wcn(13): ").getOrElse(13)
    val wfs = InputUtil.readInt("wfs(40): ").getOrElse(40)
    new scorer.KindaiScorer(wbp, wcn, wfs)
  }
}
