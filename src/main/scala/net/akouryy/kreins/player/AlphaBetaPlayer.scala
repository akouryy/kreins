package net.akouryy.kreins
package player

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import encoder.PlacementTableEncoder
import game.Board
import util.{ExtInt, InputUtil}
import scorer.Scorer
import strategy.{AlphaBetaSearch, CheckmateSearch, DyagsekiSearch}

final class AlphaBetaPlayer(
  val scorer: Scorer,
  val depth: Int,
  val dys: Map[Board, List[(Byte, Int)]]
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

    for(m <- dysSearch.bestMove(board)) return m

    val rest = board.countEmpty

    val searcher = new AlphaBetaSearch(scorer,
      if(time < 10000) 3
      else if(time < 30000 || rest >= 52) 5
      else 7
    )

    if(rest <= 25 || absolutelyWin) {
      val maxTimeMS =
        if(rest <= 24 || absolutelyWin) ((time - 1000) / 4).clampHigh(1000)
        else ((time - 10000) / 4).clampHigh(500)
      cmSearch.run(board, maxTimeMS) match {
        case WillWin(stone) =>
          println(s"win!!! $stone (in $rest)")
          absolutelyWin = true
          stone
        case WillLose =>
          println(s"lose!!! (in $rest)")
          if(resign) -1 else searcher.bestMove(board)
        case Timeout =>
          println(s"timeout!!! ($maxTimeMS ms, ${cmSearch.nNodes} nodes, ${cmSearch.nLoops} loops)")
          searcher.bestMove(board)
        case notReachHere =>
          println(s"notReachHere!!! $notReachHere")
          searcher.bestMove(board)
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