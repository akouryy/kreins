package net.akouryy.kreins
package scorer

import game.{Board, LightBoard}

object CellScorer extends Scorer {
  /**
    * https://web.archive.org/web/20181106153744/http://www.geocities.co.jp/SiliconValley-Bay/4543/Osero/Value/Value-2.html
    */
  def panelScore(p: Long) = {
    // @formatter:off
    (
      (p >>  0 & 1) *  45 + (p >>  1 & 1) * -46 + (p >>  2 & 1) *   3 + (p >>  3 & 1) *  -1
    + (p >>  4 & 1) *  -1 + (p >>  5 & 1) *   3 + (p >>  6 & 1) * -46 + (p >>  7 & 1) *  45
    + (p >>  8 & 1) * -46 + (p >>  9 & 1) * -56 + (p >> 10 & 1) *  -1 + (p >> 11 & 1) *  -3
    + (p >> 12 & 1) *  -3 + (p >> 13 & 1) *  -1 + (p >> 14 & 1) * -56 + (p >> 15 & 1) * -46
    + (p >> 16 & 1) *   3 + (p >> 17 & 1) *  -1 + (p >> 18 & 1) *   2 + (p >> 19 & 1) *  -1
    + (p >> 20 & 1) *  -1 + (p >> 21 & 1) *   2 + (p >> 22 & 1) *  -1 + (p >> 23 & 1) *   3
    + (p >> 24 & 1) *  -1 + (p >> 25 & 1) *  -3 + (p >> 26 & 1) *  -1 + (p >> 27 & 1) *   0
    + (p >> 28 & 1) *   0 + (p >> 29 & 1) *  -1 + (p >> 30 & 1) *  -3 + (p >> 31 & 1) *  -1
    + (p >> 32 & 1) *  -1 + (p >> 33 & 1) *  -3 + (p >> 34 & 1) *  -1 + (p >> 35 & 1) *   0
    + (p >> 36 & 1) *   0 + (p >> 37 & 1) *  -1 + (p >> 38 & 1) *  -3 + (p >> 39 & 1) *  -1
    + (p >> 40 & 1) *   3 + (p >> 41 & 1) *  -1 + (p >> 42 & 1) *   2 + (p >> 43 & 1) *  -1
    + (p >> 44 & 1) *  -1 + (p >> 45 & 1) *   2 + (p >> 46 & 1) *  -1 + (p >> 47 & 1) *   3
    + (p >> 48 & 1) * -46 + (p >> 49 & 1) * -56 + (p >> 50 & 1) *  -1 + (p >> 51 & 1) *  -3
    + (p >> 52 & 1) *  -3 + (p >> 53 & 1) *  -1 + (p >> 54 & 1) * -56 + (p >> 55 & 1) * -46
    + (p >> 56 & 1) *  45 + (p >> 57 & 1) * -46 + (p >> 58 & 1) *   3 + (p >> 59 & 1) *  -1
    + (p >> 60 & 1) *  -1 + (p >> 61 & 1) *   3 + (p >> 62 & 1) * -46 + (p >> 63 & 1) *  45
    )
    // @formatter:on
  }

  def score(board: LightBoard) =
    (panelScore(board.fst) - panelScore(board.snd)).toInt

  def score(board: Board): Int = score(board.toLightBoard)
}
