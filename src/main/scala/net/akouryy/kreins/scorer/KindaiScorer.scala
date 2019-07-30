package net.akouryy.kreins
package scorer

import game.{Board, LightBoard}

/**
  * https://www.info.kindai.ac.jp/~takasi-i/thesis/2012_09-1-037-0133_S_Shiota_thesis.pdf
  */
final class KindaiScorer(val wbp: Int, val wcn: Int, val wfs: Int) extends Scorer {
  val mountCellScore = CellScorer.panelScore(0x000000000000007EL)

  def score(board: Board) = {
    CellScorer.score(board.unfixedBoard) * wbp +
      (board.possPlaceable.popcount - board.pass.possPlaceable.popcount) * wcn +
      (board.fst.fixedRectangleCount - board.snd.fixedRectangleCount) * wfs +
      mountDiff(board.toLightBoard)
  }

  @inline def mountDiff(board: LightBoard) = {
    val f = board.fst
    val s = board.snd

    val a = {
      val k = (f | s) & 0x0000000000000081L
      if((~f & 0x000000000000007EL | k) == 0) 1
      else if((~s & 0x000000000000007EL | k) == 0) -1
      else 0
    }
    val b = {
      val k = (f | s) & 0x8100000000000000L
      if((~f & 0x7E00000000000000L | k) == 0) 1
      else if((~s & 0x7E00000000000000L | k) == 0) -1
      else 0
    }
    val c = {
      val k = (f | s) & 0x0100000000000001L
      if((~f & 0x0001010101010100L | k) == 0) 1
      else if((~s & 0x0001010101010100L | k) == 0) -1
      else 0
    }
    val d = {
      val k = (f | s) & 0x8000000000000080L
      if((~f & 0x0080808080808000L | k) == 0) 1
      else if((~s & 0x0080808080808000L | k) == 0) -1
      else 0
    }
    (a + b + c + d) * (wfs * 3 - mountCellScore)
  }
}
