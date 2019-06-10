package net.akouryy.kreins
package scorer

import game.Board

trait Scorer {
  def score(board: Board): Int
}
