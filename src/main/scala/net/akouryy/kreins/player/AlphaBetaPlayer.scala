package net.akouryy.kreins
package player

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import encoder.PlacementTableEncoder
import game.Board
import scorer.Scorer
import strategy.{AlphaBetaSearch, CheckmateSearch, DyagsekiSearch}
import util.ConsoleUtil.Ansi
import util.InputUtil

final class AlphaBetaPlayer(
  val scorer: Scorer,
  val depth: Int,
  val dys: PlacementTableEncoder.PlacementTable
) extends Player {
  var absolutelyWin = false

  var cmSearch = new CheckmateSearch(isDrawOK = false)

  val dysSearch = new DyagsekiSearch(dys)

  override def reset() = {
    absolutelyWin = false
    cmSearch = new CheckmateSearch(isDrawOK = false)
  }

  def think(board: Board, resign: Boolean, time: Int): Int = {
    import CheckmateSearch._

    val rest = board.countEmpty

    if(rest >= 30) for(m <- dysSearch.bestMove(board)) return m

    val searcher = new AlphaBetaSearch(scorer,
      if(time < 20000) 3
      else if(time < 40000) 5
      else 7
    )

    if(rest <= 25 || absolutelyWin) {
      val maxTimeMS =
        if(rest <= 19 || absolutelyWin) {
          if(time < 10000) time / 5 else 2000
        } else if(rest <= 21) { // 20 or 21!!!
          if(time < 10000) time / 5 else if(time < 18000) time - 8000 else 10000
        } else {
          if(time < 20000) time / 20 else 1000
        }
      val (result, stone) = cmSearch.run(board, maxTimeMS.toLong)
      if(Kreins.isDebug) {
        result match {
          case WillWin =>
            println(Ansi.bOrange(s"win!!! $stone (in $rest)"))
          case WillLose =>
            println(Ansi.fOrange(s"lose!!! $stone (in $rest)"))
          case Timeout =>
            println(s"timeout!!! $stone ($maxTimeMS ms, ${cmSearch.nNodes} " +
              s"nodes, ${cmSearch.nLoops} loops)")
        }
      }
      absolutelyWin = result == WillWin
      if(result == Timeout) {
        searcher.bestMove(board)
      } else {
        stone
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
      val dysFile = InputUtil.readLineWithRetry("dys.gz file: ")
      new AlphaBetaPlayer(scorer.CellScorer, depth, PlacementTableEncoder.decode(
        new GZIPInputStream(new FileInputStream(dysFile))
      ))
    }

    val nickname = "AlphaBetaPlayer/CellScore"
  }

  object WithKindaiScore extends PlayerGenerator[AlphaBetaPlayer] {
    def fromStdin() = {
      val scr = scorer.KindaiScorer.fromStdin()
      val depth = InputUtil.readInt("full search depth(3): ").getOrElse(3)
      val dysFile = InputUtil.readLineWithRetry("dys.gz file: ")
      new AlphaBetaPlayer(scr, depth, PlacementTableEncoder.decode(
        new GZIPInputStream(new FileInputStream(dysFile))
      ))
    }

    val nickname = "AlphaBetaPlayer/KindaiScore"
  }

  /*object WithPatternScore extends PlayerGenerator[AlphaBetaPlayer] {
    def fromStdin() = {
      val scr = scorer.PatternScorer.fromStdin()
      val depth = InputUtil.readInt("full search depth(3): ").getOrElse(3)
      new AlphaBetaPlayer(scr, depth)
    }

    val nickname = "AlphaBetaPlayer/PatternScore"
  }*/

}