package net.akouryy.kreins
package fl

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import game.Board
import net.akouryy.kreins.encoder.PlacementTableEncoder
import player.AlphaBetaPlayer
import scorer.KindaiScorer

class Client(host: String, port: Int, name: String, zysFile: String) {
  val conn = Connector(host, port)

  sealed trait GameState

  case object Waiting extends GameState

  final case class Playing(board: Board, timeMS: Int) extends GameState

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
        val Playing(board, timeMS) = state

        val pos = player.think(board, resign = false, timeMS)
        if(pos == -1) {
          w(Pass.messageString)
          state = Playing(board.pass, timeMS)
        } else {
          w(Move(pos).messageString)
          state = Playing(board.place(pos), timeMS)
        }
      }

      if(s == null) sys.exit(2)

      parse(s).forall {
        case Start(isBlack, herName, timeMS) =>
          state = Playing(Board.InitialBoard, timeMS)
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
          val Playing(board, timeMS) = state
          state = Playing(board.place(pos), timeMS)
          turn()
          true
        case Pass =>
          val Playing(board, timeMS) = state
          state = Playing(board.pass, timeMS)
          turn()
          true
        case Ack(timeMS) =>
          val Playing(board, _) = state
          state = Playing(board, timeMS)
          println(state)
          true
      }
    }
  }
}
