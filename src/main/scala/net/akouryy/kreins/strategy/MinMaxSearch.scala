package net.akouryy.kreins
package strategy

import game.Board
import scorer.Scorer

class MinMaxSearch(val scorer: Scorer, val depth: Int) {
  def bestMove(board: Board) = {
    val pp = board.possPlaceable.code

    if(pp == 0) {
      -1
    } else {
      (0 to 63).filter(i => (pp >>> i & 1) == 1).minBy { i =>
        bestScore(board.place(i))
      }
    }
  }

  def bestScore(board: Board, level: Int = 0): Int =
    if(level == depth) {
      (scorer.score(board) * (0.8 + 0.1 * scala.util.Random.nextInt(5))).toInt
    } else {
      val pp = board.possPlaceable.code
      if(pp == 0) {
        -bestScore(board.pass, level + 1)
      } else {
        (0 to 63).filter(i => (pp >>> i & 1) == 1).map { i =>
          -bestScore(board.place(i), level + 1)
        }.max
      }
    }
}
