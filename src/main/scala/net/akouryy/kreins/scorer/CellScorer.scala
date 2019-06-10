package net.akouryy.kreins
package scorer

import game.Board

object CellScorer extends Scorer {
  /**
    * https://web.archive.org/web/20181106153744/http://www.geocities.co.jp/SiliconValley-Bay/4543/Osero/Value/Value-2.html
    */
  val cellScores = Seq(
    45, -11, 4, -1, -1, 4, -11, 45,
    -11, -16, -1, -3, -3, -1, -16, -11,
    4, -1, 2, -1, -1, 2, -1, 4,
    -1, -3, -1, 0, 0, -1, -3, -1,
    -1, -3, -1, 0, 0, -1, -3, -1,
    4, -1, 2, -1, -1, 2, -1, 4,
    -11, -16, -1, -3, -3, -1, -16, -11,
    45, -11, 4, -1, -1, 4, -11, 45
  )

  def score(board: Board) = {
    (0 to 63).map { i =>
      if((board.fst.code >> i & 1) == 1) cellScores(i)
      else if((board.snd.code >> i & 1) == 1) -cellScores(i)
      else 0
    }.sum
  }
}
