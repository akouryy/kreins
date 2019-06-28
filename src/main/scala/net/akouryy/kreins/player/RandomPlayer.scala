package net.akouryy.kreins
package player

import scala.util.Random

import game.Board
import util.BitUtil

object RandomPlayer extends Player {
  def think(b: Board, resign: Boolean, time: Int) = Player.randomThink(b)

  object Generator extends PlayerGenerator[RandomPlayer.type] {
    def fromStdin() = RandomPlayer

    val nickname = "Random"
  }

}
