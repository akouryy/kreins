package net.akouryy.kreins
package player

import scala.util.Random

import game.Board
import strategy.CheckmateSearch
import util.BitUtil

object RandomThenCheckmatePlayer extends Player {
  def think(b: Board) = {
    import CheckmateSearch._
    if(b.countEmpty <= 20) {
      new CheckmateSearch(b, false).run match {
        case WillWin(stone) => stone
        case _ => Player.randomThink(b)
      }
    } else {
      Player.randomThink(b)
    }
  }

  object Generator extends PlayerGenerator[RandomThenCheckmatePlayer.type] {
    def fromStdin() = RandomThenCheckmatePlayer

    val nickname = "RandomThenCheckmate"
  }

}
