package net.akouryy.kreins
package player

import scala.io.StdIn
import game.Board
import net.akouryy.kreins.util.InputUtil
import scorer.Scorer
import strategy.{CheckmateSearch, AlphaBetaSearch}

class AlphaBetaPlayer(val scorer: Scorer, val depth: Int) extends Player {
  val fss = new AlphaBetaSearch(scorer, depth)

  def think(b: Board, resign: Boolean) = {
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

object AlphaBetaPlayer {

  object WithCellScore extends PlayerGenerator[AlphaBetaPlayer] {
    def fromStdin() = {
      val depth = InputUtil.readInt("full search depth(3): ").getOrElse(3)
      new AlphaBetaPlayer(scorer.CellScorer, depth)
    }

    val nickname = "AlphaBetaPlayer/CellScore"
  }

  object WithKindaiScore extends PlayerGenerator[AlphaBetaPlayer] {
    def fromStdin() = {
      val scr = scorer.KindaiScorer.fromStdin()
      val depth = InputUtil.readInt("full search depth(3): ").getOrElse(3)
      new AlphaBetaPlayer(scr, depth)
    }

    val nickname = "AlphaBetaPlayer/KindaiScore"
  }

  object WithPatternScore extends PlayerGenerator[AlphaBetaPlayer] {
    def fromStdin() = {
      val scr = scorer.PatternScorer.fromStdin()
      val depth = InputUtil.readInt("full search depth(3): ").getOrElse(3)
      new AlphaBetaPlayer(scr, depth)
    }

    val nickname = "AlphaBetaPlayer/PatternScore"
  }

}