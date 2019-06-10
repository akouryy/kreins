package net.akouryy.kreins
package scorer

import game.Board

/**
  * https://www.info.kindai.ac.jp/~takasi-i/thesis/2012_09-1-037-0133_S_Shiota_thesis.pdf
  */
class KindaiScorer(val wbp: Int, val wcn: Int) extends Scorer {
  def score(board: Board) = {
    CellScorer.score(board) * wbp + board.possPlaceable.popcount * wcn
  }

}
