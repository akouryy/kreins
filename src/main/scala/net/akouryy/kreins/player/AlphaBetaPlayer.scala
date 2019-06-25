package net.akouryy.kreins
package player

import scala.io.StdIn
import game.Board
import net.akouryy.kreins.util.InputUtil
import scorer.Scorer
import strategy.{CheckmateSearch, AlphaBetaSearch}

class AlphaBetaPlayer(val scorer: Scorer, val depth: Int) extends Player {
  val fss = new AlphaBetaSearch(scorer, depth)

  var absolutelyWin = false

  override def reset() = {
    absolutelyWin = false
  }

  def think(board: Board, resign: Boolean) = {
    import CheckmateSearch._
    val rest = board.countEmpty
    if(rest <= 50 || /*rest / 2 % 3 == 0 ||*/ absolutelyWin) {
      new CheckmateSearch(
        board,
        isDrawOK = false,
        maxTimeMS = if(rest <= 20 || absolutelyWin) 500 else if(rest <= 40) 200 else 100
      ).run match {
        case WillWin(stone) =>
          println(s"absolutely win! $stone (in $rest)")
          absolutelyWin = true
          stone
        case WillLose =>
          if(resign) -1 else fss.bestMove(board)
        case _ => fss.bestMove(board)
      }
    } else {
      fss.bestMove(board)
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