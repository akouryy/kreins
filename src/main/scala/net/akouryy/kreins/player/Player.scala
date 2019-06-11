package net.akouryy.kreins
package player

import scala.util.Random

import game.Board
import util.BitUtil

trait Player {
  def think(b: Board, resign: Boolean): Int
}

trait PlayerGenerator[P <: Player] {
  def fromStdin(): P

  val nickname: String
}

object Player {
  val generators = Seq(
    RandomPlayer.Generator,
    MinMaxPlayer.WithCellScore,
    MinMaxPlayer.WithKindaiScore,
    AlphaBetaPlayer.WithCellScore,
    AlphaBetaPlayer.WithKindaiScore
  )

  def randomThink(b: Board) = {
    val pp = b.possPlaceable.code
    val idx = Random.nextInt(BitUtil.popcount(pp))

    (0 to 63).find(p =>
      (pp >>> p & 1) == 1 && BitUtil.popcount(pp & (1L << p) - 1) == idx
    ).getOrElse(0)
  }
}
