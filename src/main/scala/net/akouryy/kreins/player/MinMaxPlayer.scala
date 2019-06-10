package net.akouryy.kreins
package player

import scala.io.StdIn
import game.Board
import net.akouryy.kreins.util.InputUtil
import scorer.Scorer
import strategy.{CheckmateSearch, MinMaxSearch}

class MinMaxPlayer(val scorer: Scorer, val depth: Int) extends Player {
  val fss = new MinMaxSearch(scorer, depth)

  def think(b: Board) = {
    import CheckmateSearch._
    if(b.countEmpty <= 23) {
      new CheckmateSearch(b, false).run match {
        case WillWin(stone) => stone
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
      print("full search depth: ")
      new MinMaxPlayer(scorer.CellScorer, StdIn.readInt())
    }

    val nickname = "MinMaxPlayer/CellScore"
  }

  object WithKindaiScore extends PlayerGenerator[MinMaxPlayer] {
    def fromStdin() = {
      print("wbp[3]: ")
      val wbp = InputUtil.readInt().getOrElse(3)
      print("wcn[10]: ")
      val wcn = InputUtil.readInt().getOrElse(10)
      print("full search depth[3]: ")
      val depth = InputUtil.readInt().getOrElse(3)
      new MinMaxPlayer(new scorer.KindaiScorer(wbp, wcn), depth)
    }

    val nickname = "MinMaxPlayer/KindaiScore"
  }

}