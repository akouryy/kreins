package net.akouryy.kreins
package scorer

import game.{Board, Panel}

/**
  * http://othello.sakipiyo.net/ryoukeiakukei.html
  */
object EdgeScorer extends Scorer {
  private[this] def edgeScore(edge: Int, e2op: Int) = {
    if(edge == 0x3c) { // block
      val x = (e2op >> 2 & 1) + (e2op >> 5 & 1)
      if(x == 2)
        3 - (3 + -1 + -1 + 3)
      else if(x == 1)
        10 - (3 + -1 + -1 + 3)
      else
        30 - (3 + -1 + -1 + 3)
    } else if(edge == 0x7e) { // mountain
      40 - (-16 + 3 + -1 + -1 + 3 + -16)
    } else if(edge == 0x3e) { // right wing
      -40 - (3 + -1 + -1 + 3 + -16)
    } else if(edge == 0x7c) { // left wing
      -40 - (-16 + 3 + -1 + -1 + 3)
    } else {
      0
    }
  }

  def score(board: Board) =
    score1(board) + score1(board.mirrorWithDiagRightDown)

  @inline private[this] def score1(board: Board) =
    score2(board.fst, board.snd) - score2(board.snd, board.fst)

  @inline private[this] def score2(p1: Panel, p2: Panel) =
    edgeScore((p1.code & 255).toInt, (p2.code >> 8 & 255).toInt) +
      edgeScore((p1.code >>> 56).toInt, (p2.code >> 48 & 255).toInt)
}
