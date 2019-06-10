package net.akouryy.kreins
package strategy

import game.Board
import scorer.Scorer

class NextScore(val scorer: Scorer) {
  def bestMove(board: Board) = {
    val pp = board.possPlaceable.code

    if(pp == 0) {
      -1
    } else {
      (0 to 63).filter(i => (pp >>> i & 1) == 1).maxBy { i =>
        scorer.score(board.place(i).pass)
      }
    }
  }
}
