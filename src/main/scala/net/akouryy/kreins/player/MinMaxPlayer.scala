package net.akouryy.kreins
package player

import scala.io.StdIn
import game.Board
import net.akouryy.kreins.util.InputUtil
import scorer.Scorer
import strategy.{CheckmateSearch, MinMaxSearch}

class MinMaxPlayer(val scorer: Scorer, val depth: Int) extends Player {
  val fss = new MinMaxSearch(scorer, depth)

  def think(b: Board, resign: Boolean, time: Int) = {
    import CheckmateSearch._
    if(b.countEmpty <= 23) {
      new CheckmateSearch(b, false, 100).run match {
        case WillWin(stone) => stone
        case WillLose =>
          if(resign) -1 else fss.bestMove(b)
        case _ => fss.bestMove(b)
      }
    } else {
      fss.bestMove(b)
    }
  }
}

object MinMaxPlayer {

  object WithCellScore extends PlayerGenerator[MinMaxPlayer] {
    def fromStdin() = {
      val depth = InputUtil.readInt("full search depth(3): ").getOrElse(3)
      new MinMaxPlayer(scorer.CellScorer, depth)
    }

    val nickname = "MinMaxPlayer/CellScore"
  }

  object WithKindaiScore extends PlayerGenerator[MinMaxPlayer] {
    def fromStdin() = {
      val scr = scorer.KindaiScorer.fromStdin()
      val depth = InputUtil.readInt("full search depth(3): ").getOrElse(3)
      new MinMaxPlayer(scr, depth)
    }

    val nickname = "MinMaxPlayer/KindaiScore"
  }

}