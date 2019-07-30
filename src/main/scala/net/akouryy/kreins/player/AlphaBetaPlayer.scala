package net.akouryy.kreins
package player

import encoder.PlacementTableEncoder
import game.Board
import scorer.Scorer
import strategy.{AlphaBetaSearch, CheckmateSearch, DyagsekiSearch}
import util.ConsoleUtil.Ansi
import util.BitUtil

final class AlphaBetaPlayer(
  val scorer: Scorer,
  val depth: Int,
  val dys: PlacementTableEncoder.CompactPTable
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

    if(rest == 0 && time >= 10000) {
      reset()
      Kreins.gc1Sec()
      return -1
    }

    if(rest >= 60 - DyagsekiSearch.MaxTurn) for(m <- dysSearch.bestMove(board)) return m

    val pp = board.possPlaceable.code
    val ppc = BitUtil.popcount(pp)

    if(ppc == 0) return -1
    if(ppc == 1) {
      return (0 to 63).find(i => (pp >> i & 1) == 1).get
    }

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
