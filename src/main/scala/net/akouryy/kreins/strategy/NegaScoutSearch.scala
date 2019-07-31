package net.akouryy.kreins
package strategy

import game.Board
import scorer.Scorer

/**
  * https://ja.wikipedia.org/wiki/Negascout
  */
final class NegaScoutSearch(val scorer: Scorer, val depth: Int) {

  import NegaScoutSearch._

  private[this] var endTimeMS = 0L

  def bestMove(board: Board, durMS: Long) = {
    val pp = board.possPlaceable.code
    this.endTimeMS = if(durMS > 0) System.currentTimeMillis + durMS else -1L

    if(pp == 0) {
      -1
    } else {
      (0 to 63).filter(i => (pp >>> i & 1) == 1).minBy { i =>
        val a = bestScore(board.place(i), 0, Int.MinValue + 10, Int.MaxValue - 10)
        a
      }
    }
  }

  def bestScore(board: Board, level: Int, alpha0: Int, beta: Int): Int = {
    if(level == depth) {
      scorer.score(board)
    } else {
      if(level <= 3 && depth >= 4 && endTimeMS > 0 && System.currentTimeMillis > endTimeMS) {
        throw TimeoutError
      }

      val pp = board.possPlaceable.code
      if(pp == 0) {
        return -bestScore(board.pass, level + 1, -beta, -alpha0)
      }
      val cs = (0 to 63).withFilter(i => (pp >>> i & 1) == 1).map(board.place)
      if(cs.size == 1) {
        return -bestScore(cs.head, level + 1, -beta, -alpha0)
      }

      var isFirst = true
      var alpha = alpha0
      var max = Int.MinValue

      cs.sortBy(_.possPlaceable.popcount).foreach { c =>
        if(isFirst) {
          val v = -bestScore(c, level + 1, -beta, -alpha)
          if(beta <= v) return v
          if(alpha < v) alpha = v
          max = v

          isFirst = false
        } else {
          var v = -bestScore(c, level + 1, -alpha - 1, -alpha) // null window search
          if(beta <= v) return v
          if(alpha < v) {
            alpha = v
            v = -bestScore(c, level + 1, -beta, -alpha)
            if(beta <= v) return v
            if(alpha < v) alpha = v
          }
          if(max < v) max = v
        }
      }

      max
    }
  }
}

object NegaScoutSearch {

  object TimeoutError extends RuntimeException("timeout")

}
