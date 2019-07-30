package net.akouryy.kreins
package player

import game.Board

trait Player {
  def think(b: Board, resign: Boolean, timeMS: Int = 100000000): Int

  def reset(): Unit = {}
}
