package net.akouryy.kreins
package game

import player.Player

case class Game(black: Player, white: Player) {
  var isBlackFst = true
  var board = Board.InitialBoard
  var resignedBy: Option[Boolean] = None

  def run(allowResign: Boolean) =
    while(!board.isEnd && resignedBy.isEmpty) runStep(allowResign)

  def runStep(allowResign: Boolean) {
    if(board.possPlaceable.code != 0) {
      val stone = (if(isBlackFst) black else white).think(board, allowResign)
      if(stone == -1) {
        resignedBy = Some(isBlackFst)
        return
      }
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
