package net.akouryy.kreins
package player

import game.Board
import strategy.CheckmateSearch

object CheckmateRandomPlayer extends Player {
  def think(b: Board, resign: Boolean, time: Int) = {
    import CheckmateSearch._
    if(b.countEmpty <= 23) {
      new CheckmateSearch(false).run(b, 100) match {
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