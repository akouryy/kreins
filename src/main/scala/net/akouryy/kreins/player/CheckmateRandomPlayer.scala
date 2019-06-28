package net.akouryy.kreins
package player

import scala.io.StdIn
import game.Board
import net.akouryy.kreins.util.InputUtil
import scorer.Scorer
import strategy.{CheckmateSearch, MinMaxSearch}

object CheckmateRandomPlayer extends Player {
  def think(b: Board, resign: Boolean, time: Int) = {
    import CheckmateSearch._
    if(b.countEmpty <= 23) {
      new CheckmateSearch(b, false, 100).run match {
        case WillWin(stone) => stone
        case WillLose =>
          if(resign) -1 else Player.randomThink(b)
        case _ => Player.randomThink(b)
      }
    } else {
      Player.randomThink(b)
    }
  }

  object Generator extends PlayerGenerator[CheckmateRandomPlayer.type] {
    def fromStdin() = CheckmateRandomPlayer

    val nickname = "CheckmateRandom"
  }

}