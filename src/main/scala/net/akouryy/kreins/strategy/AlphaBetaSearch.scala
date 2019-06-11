package net.akouryy.kreins
package strategy

import game.Board
import scorer.Scorer

class AlphaBetaSearch(val scorer: Scorer, val depth: Int) {
  def bestMove(board: Board) = {
    val pp = board.possPlaceable.code

    if(pp == 0) {
      -1
    } else {
      (0 to 63).filter(i => (pp >>> i & 1) == 1).minBy { i =>
        bestScore(board.place(i), 0, Int.MinValue, Int.MaxValue)
      }
    }
  }

  def bestScore(board: Board, level: Int, alpha0: Int, beta: Int): Int = {
    if(level == depth) {
      (scorer.score(board) * (0.8 + 0.1 * scala.util.Random.nextInt(5))).toInt
    } else {
      val pp = board.possPlaceable.code
      if(pp == 0) {
        -bestScore(board.pass, level + 1, -beta, -alpha0)
      } else {
        var alpha = alpha0
        var ret = Int.MinValue

        var i = 0
        while(i < 64) {
          if((pp >>> i & 1) == 1) {
            val v = -bestScore(board.place(i), level + 1, -beta, -alpha)
            if(ret < v) {
              ret = v
              if(alpha < v) alpha = v
              if(beta <= v) return v
            }
          }
          i += 1
        }
        ret
      }
    }
  }
}
