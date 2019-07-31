package net.akouryy.kreins
package scorer

import game.{Board, LightBoard}

object CellScorer extends Scorer {
  /**
    * https://web.archive.org/web/20181106153744/http://www.geocities.co.jp/SiliconValley-Bay/4543/Osero/Value/Value-2.html
    */
  def panelScore(p: Long) = {
    // @formatter:off
    ((p       & 1) + (p >>  7 & 1) + (p >> 56 & 1) + (p >> 63 & 1)).toInt * 45 +
    ((p >>  1 & 1) + (p >>  6 & 1) + (p >>  8 & 1) + (p >> 15 & 1) +
     (p >> 48 & 1) + (p >> 55 & 1) + (p >> 57 & 1) + (p >> 62 & 1)).toInt * -16 +
    ((p >>  2 & 1) + (p >>  5 & 1) + (p >> 16 & 1) + (p >> 23 & 1) +
     (p >> 40 & 1) + (p >> 47 & 1) + (p >> 58 & 1) + (p >> 61 & 1)).toInt * 3 +
    ((p >>  3 & 1) + (p >>  4 & 1) + (p >> 24 & 1) + (p >> 31 & 1) +
     (p >> 32 & 1) + (p >> 39 & 1) + (p >> 59 & 1) + (p >> 60 & 1)).toInt * -1 +
    ((p >>  9 & 1) + (p >> 14 & 1) + (p >> 49 & 1) + (p >> 54 & 1)).toInt * -26 +
    ((p >> 10 & 1) + (p >> 13 & 1) + (p >> 17 & 1) + (p >> 22 & 1) +
     (p >> 41 & 1) + (p >> 46 & 1) + (p >> 50 & 1) + (p >> 53 & 1)).toInt * -1 +
    ((p >> 11 & 1) + (p >> 12 & 1) + (p >> 25 & 1) + (p >> 30 & 1) +
     (p >> 33 & 1) + (p >> 38 & 1) + (p >> 51 & 1) + (p >> 52 & 1)).toInt * -3 +
    ((p >> 18 & 1) + (p >> 21 & 1) + (p >> 42 & 1) + (p >> 45 & 1)).toInt * 2 +
    ((p >> 19 & 1) + (p >> 20 & 1) + (p >> 26 & 1) + (p >> 29 & 1) +
     (p >> 34 & 1) + (p >> 37 & 1) + (p >> 43 & 1) + (p >> 44 & 1)).toInt * -1
    // @formatter:on
  }

  def score(board: LightBoard) =
    panelScore(board.fst) - panelScore(board.snd)

  def score(board: Board): Int = score(board.toLightBoard)
}
