package net.akouryy.kreins
package fl

import game.Board
import player.AlphaBetaPlayer
import scorer.KindaiScorer

class Client(host: String, port: Int, name: String) {
  val conn = Connector(host, port)

  def run() = {
    import Command._

    val player =
      new AlphaBetaPlayer(new KindaiScorer(3, 13, 40), 5)
    var board: Option[Board] = None

    conn.connect { w =>
      w(Open(name).messageString)
    } { (s, w) =>
      def turn() = {
        val pos = player.think(board.get, resign = false)
        if(pos == -1)
          w(Pass.messageString)
        else {
          w(Move(pos).messageString)
          board = Some(board.get.place(pos))
        }
      }

      if(s == null) sys.exit(0)

      parse(s).forall {
        case Start(isBlack, herName, time) =>
          board = Some(Board.InitialBoard)
          if(isBlack) turn()
          true
        case End(result, myStone, herStone, reason) =>
          board = None
          true
        case Bye(scores) =>
          false
        case Move(pos) =>
          board = Some(board.get.place(pos))
          turn()
          true
        case Pass =>
          board = Some(board.get.pass)
          turn()
          true
        case Ack(time) =>
          true
      }
    }
  }
}
