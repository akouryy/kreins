package net.akouryy.kreins
package fl

import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import scala.collection.mutable.{IndexedSeq => MutIndexedSeq}

import game.Board
import encoder.PlacementTableEncoder
import util.ConsoleUtil.Ansi
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
        new KindaiScorer(1, 20, 50),
        depth = 6,
        zys
      )

    var stats = if(Kreins.isDebug) initStats else null

    var state: GameState = Waiting
    var amIBlack = false

    Kreins.gc1Sec()

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
        case Start(isBlack, _, timeMS) =>
          state = Playing(-1, Board.InitialBoard, timeMS)
          amIBlack = isBlack
          if(isBlack) turn()
          true
        case End(result, _, _, _) =>
          if(Kreins.isDebug) {
            result match {
              case EndResult.Win =>
                if(amIBlack)
                  stats = stats.copy(bWin = stats.bWin + 1)
                else
                  stats = stats.copy(wWin = stats.wWin + 1)
              case EndResult.Lose =>
                if(amIBlack)
                  stats = stats.copy(bLose = stats.bLose + 1)
                else
                  stats = stats.copy(wLose = stats.wLose + 1)
              case EndResult.Draw =>
                if(amIBlack)
                  stats = stats.copy(bLose = stats.bLose + 1)
                else
                  stats = stats.copy(wLose = stats.wLose + 1)
            }
            println(stats.toLongString)
          }
          state = Waiting
          player.reset()
          System.gc()
          true
        case Bye(_) =>
          false
        case Move(pos) =>
          val Playing(_, board, timeMS) = state
          state = Playing(pos, board.place(pos), timeMS)
          turn()
          System.gc()
          true
        case Pass =>
          val Playing(_, board, timeMS) = state
          state = Playing(-1, board.pass, timeMS)
          turn()
          System.gc()
          true
        case Ack(timeMS) =>
          val Playing(lastPos, board, _) = state
          state = Playing(lastPos, board, timeMS)
          if(Kreins.isDebug) {
            stats.ackSum(60 - board.countEmpty) += timeMS
            stats.ackCnt(60 - board.countEmpty) += 1
            println(stats)
            println(state)
          }
          true
      }
    }
  }
}

object Client {

  sealed trait GameState

  case object Waiting extends GameState

  final case class Playing(lastPos: Int, board: Board, timeMS: Int)
    extends GameState {
    override def toString = s"Playing(${timeMS}ms, ${board.toString(lastPos)})"
  }

  final case class Stats(
    bWin: Int,
    bDraw: Int,
    bLose: Int,
    wWin: Int,
    wDraw: Int,
    wLose: Int,
    ackSum: MutIndexedSeq[Int],
    ackCnt: MutIndexedSeq[Int]
  ) {
    val win = bWin + wWin
    val draw = bDraw + wDraw
    val lose = bLose + wLose
    val games = win + draw + lose
    val bGames = bWin + bDraw + bLose
    val wGames = wWin + wDraw + wLose

    override def toString =
      Ansi.fSky(
        f"""WIN  : me $win%03d-($draw%03d)-$lose%03d op / $games%03d games
           |BLACK: me $bWin%03d-($bDraw%03d)-$bLose%03d op / $bGames%03d games
           |WHITE: me $wWin%03d-($wDraw%03d)-$wLose%03d op / $wGames%03d games""".stripMargin
      )

    def toLongString =
      Ansi.fSky(
        f"""--------------------------------------------------------------------------------
           |WIN  : me $win%03d-($draw%03d)-$lose%03d op / $games%03d games
           |BLACK: me $bWin%03d-($bDraw%03d)-$bLose%03d op / $bGames%03d games
           |WHITE: me $wWin%03d-($wDraw%03d)-$wLose%03d op / $wGames%03d games
           |
           |ACKs: ${
          ackSum.zip(ackCnt).map {
            case (s, c) => if(c == 0) "-----" else f"${s / c}%05d"
          }.mkString(" ")
        }
           |--------------------------------------------------------------------------------""".stripMargin
      )
  }

  def initStats =
    Stats(0, 0, 0, 0, 0, 0, MutIndexedSeq.fill(61)(0), MutIndexedSeq.fill(61)(0))
}
