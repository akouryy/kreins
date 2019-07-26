package net.akouryy.kreins
package fl

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import game.Board
import net.akouryy.kreins.encoder.PlacementTableEncoder
import player.AlphaBetaPlayer
import scorer.KindaiScorer

final class Client(host: String, port: Int, name: String, zysFile: String) {

  import Client._

  val conn = Connector(host, port)

  def run() = {
    import Command._

    val zys = PlacementTableEncoder.decode(
      new GZIPInputStream(new FileInputStream(zysFile))
    )

    val player =
      new AlphaBetaPlayer(
        new KindaiScorer(3, 13, 40),
        depth = 6,
        zys
      )

    var state: GameState = Waiting

    conn.connect { w =>
      w(Open(name).messageString)
    } { (s, w) =>
      def turn() = {
        val Playing(_, board, timeMS) = state

        val pos = player.think(board, resign = false, timeMS)
        if(pos == -1) {
          w(Pass.messageString)
          state = Playing(-1, board.pass, timeMS)
        } else {
          w(Move(pos).messageString)
          state = Playing(pos, board.place(pos), timeMS)
        }
      }

      if(s == null) sys.exit(2)

      parse(s).forall {
        case Start(isBlack, herName, timeMS) =>
          state = Playing(-1, Board.InitialBoard, timeMS)
          if(isBlack) turn()
          true
        case End(result, myStone, herStone, reason) =>
          state = Waiting
          player.reset()
          System.gc()
          true
        case Bye(scores) =>
          false
        case Move(pos) =>
          val Playing(_, board, timeMS) = state
          state = Playing(pos, board.place(pos), timeMS)
          turn()
          true
        case Pass =>
          val Playing(_, board, timeMS) = state
          state = Playing(-1, board.pass, timeMS)
          turn()
          true
        case Ack(timeMS) =>
          val Playing(lastPos, board, _) = state
          state = Playing(lastPos, board, timeMS)
          println(state)
          true
      }
    }
  }
}

object Client {

  sealed trait GameState

  case object Waiting extends GameState

  final case class Playing(lastPos: Int, board: Board, timeMS: Int) extends GameState {
    override def toString = s"Playing(${timeMS}ms, ${board.toString(lastPos)})"
  }

}
