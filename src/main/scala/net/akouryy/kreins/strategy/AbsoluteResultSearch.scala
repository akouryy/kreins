package net.akouryy.kreins
package strategy

import model.Board
import model.Panel
import scala.collection.mutable

class AbsoluteResultSearch(initialBoard: Board) {
  private[this] val WIN_MIN: Short = 10000

  def run = eval(initialBoard)

  def evalEndGame(b: Board) = {
    val cb = b.countBlack.toShort
    val cw = b.countWhite.toShort
    if(cb > cw) {
      // メタ読みで、差が小さい方が相手AIが受け入れる(この手を打ってくれる)可能性が高いと考える
      (Short.MaxValue - (cb - cw)).toShort
    } else if(cb == cw) {
      0: Short
    } else {
      // メタ読みで、差が小さい方が相手AIが見逃す(別の手を打ってくれる)可能性が高いと考える
      (Short.MinValue + (cw - cb)).toShort
    }
  }

  def eval(b: Board): Short = {
    val p = b.possPlaceable
    if(p.code == 0) {
      val c = b.pass
      val q = c.possPlaceable

      if(q.code == 0) {
        // 終局
        evalEndGame(b)
      } else {
        // 自分はパス
        var worstScore = Short.MaxValue
        for(i <- 0 to 63) if((q.code >> i & 1) == 1) {
          worstScore = Math.min(worstScore, eval(c.place(i, c.possToFlip(i)))).toShort
        }
        worstScore
      }
    } else {
      val poss =
        mutable.ArrayBuffer[(Int, Panel, Board)]()

      for(i <- 0 to 63) if((p.code >> i & 1) == 1) {
        val c = b.place(i, b.possToFlip(i))
        val p = c.possPlaceable
        poss += ((p.popcount, p, c))
      }

      var bestScore = Short.MinValue

      poss.sortBy(_._1).foreach { case (_, q, c) =>
        if(q.code == 0) {
          // 相手パス
          bestScore = Math.max(bestScore, eval(c.pass)).toShort
        } else {
          var worstScore = Short.MaxValue
          for(i <- 0 to 63) if((q.code >> i & 1) == 1) {
            worstScore = Math.min(worstScore, eval(c.place(i, c.possToFlip(i)))).toShort
          }
          if(worstScore >= WIN_MIN) {
            return worstScore
          }
          bestScore = Math.max(bestScore, worstScore).toShort
        }
      }

      bestScore
    }
  }
}
