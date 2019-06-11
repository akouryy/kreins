package net.akouryy.kreins
package strategy

import game.Board
import util.BitUtil
import scorer.Scorer

class AlphaBetaSearch(val scorer: Scorer, val depth: Int) {
  def bestMove(board: Board) = {
    val pp = board.possPlaceable.code

    if(pp == 0) {
      -1
    } else {
      (0 to 63).filter(i => (pp >>> i & 1) == 1).minBy { i =>
        /* `α = Int.MinValue`だと`-α`がオーバーフロー!!! */
        bestScore(board.place(i), 0, Int.MinValue + 1, Int.MaxValue)
      }
    }
  }

  def bestScore(board: Board, level: Int, alpha0: Int, beta: Int): Int = {
    if(level == depth) {
      (scorer.score(board) * (0.8 + 0.1 * scala.util.Random.nextInt(5))).toInt
    } else {
      var pp = board.possPlaceable.code
      if(pp == 0) {
        -bestScore(board.pass, level + 1, -beta, -alpha0)
      } else {
        var alpha = alpha0

        while(pp != 0) {
          val i = BitUtil.firstHighBitPos(pp)
          pp &= ~(1L << i)
          alpha = alpha.max(-bestScore(board.place(i), level + 1, -beta, -alpha))
          if(beta <= alpha) return alpha
        }
        alpha
      }
    }
  }
}
