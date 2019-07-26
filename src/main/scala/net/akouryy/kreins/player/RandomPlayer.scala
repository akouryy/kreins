package net.akouryy.kreins
package player

import game.Board

object RandomPlayer extends Player {
  def think(b: Board, resign: Boolean, time: Int) = Player.randomThink(b)

  object Generator extends PlayerGenerator[RandomPlayer.type] {
    def fromStdin() = RandomPlayer

    val nickname = "Random"
  }

}
