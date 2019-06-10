package net.akouryy.kreins
package game

import player.Player

case class Game(black: Player, white: Player) {
  var isBlackFst = true
  var board = Board.initialBoard

  def run() = while(!board.isEnd) runStep()

  def runStep() {
    if(board.possPlaceable.code != 0) {
      val stone = (if(isBlackFst) black else white).think(board)
      if((board.possPlaceable.code >> stone & 1) == 0) {
        throw new RuntimeException(s"Invalid stone: $stone ($isBlackFst)\n$board")
      }
      board = board.place(stone)
    } else {
      board = board.pass
    }
    isBlackFst = !isBlackFst
  }

  lazy val blackBoard = if(isBlackFst) board else board.pass
}
