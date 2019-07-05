package net.akouryy.kreins
package player

import scala.io.StdIn
import game.Board
import net.akouryy.kreins.util.InputUtil
import scorer.Scorer
import strategy.{CheckmateSearch, AlphaBetaSearch}

class AlphaBetaPlayer(val scorer: Scorer, val depth: Int) extends Player {
  var absolutelyWin = false

  var cmSearch = new CheckmateSearch(isDrawOK = false)

  override def reset() = {
    absolutelyWin = false
    cmSearch = new CheckmateSearch(isDrawOK = false)
  }

  def think(board: Board, resign: Boolean, time: Int) = {
    import CheckmateSearch._

    val rest = board.countEmpty

    val searcher = new AlphaBetaSearch(scorer,
      if(time < 10000) 3
      else if(time < 30000 || rest >= 52) 5
      else 6
    )

    if(rest <= 25 || absolutelyWin) {
      cmSearch.run(
        board,
        maxTimeMS =
          if(rest <= 24 || absolutelyWin) (time / 2) max 50 min 500
          else if(rest <= 30) (time / 5) max 50 min 200
          else 50
      ) match {
        case WillWin(stone) =>
          println(s"win!!! $stone (in $rest)")
          absolutelyWin = true
          stone
        case WillLose =>
          println(s"lose!!! (in $rest)")
          if(resign) -1 else searcher.bestMove(board)
        case _ => searcher.bestMove(board)
      }
    } else {
      searcher.bestMove(board)
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