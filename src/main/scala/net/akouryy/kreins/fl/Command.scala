package net.akouryy.kreins.fl

import scala.util.Try

sealed trait Command

object Command {

  sealed trait ClientCommand extends Command {
    def messageString = messageParts.mkString(" ")

    def messageParts: Seq[String]
  }

  sealed trait ServerCommand extends Command

  final case class Open(playerName: String) extends ClientCommand {
    def messageParts = List("OPEN", playerName)
  }

  final case class Move(pos: Int) extends ClientCommand with ServerCommand {
    def messageParts = List("MOVE", "" + ('A' + pos / 8).toChar + (pos % 8 + 1))
  }

  final case object Pass extends ClientCommand with ServerCommand {
    def messageParts = List("MOVE", "PASS")
  }

  final case class Start(isBlack: Boolean, herName: String, time: Int) extends ServerCommand

  final case class End(result: EndResult, myStone: Int, herStone: Int, reason: String)
    extends ServerCommand

  final case class Bye(scores: Seq[String]) extends ServerCommand

  final case class Ack(time: Int) extends ServerCommand

  def parse(str: String): Option[ServerCommand] = {
    str.stripLineEnd.split(' ') match {
      case Array("START", bw @ ("BLACK" | "WHITE"), name, time) =>
        Some(Start(bw == "BLACK", name, Try(time.toInt).getOrElse(60000)))
      case Array("END", res @ ("WIN" | "LOSE" | "TIE"), n, m, reason) =>
        Some(End(
          res match {
            case "WIN" => EndResult.Win
            case "LOSE" => EndResult.Lose
            case "TIE" => EndResult.Draw
            case _ => Console.err.println(s"unknown result: '$res'"); EndResult.Draw
          },
          Try(n.toInt).getOrElse(-1),
          Try(m.toInt).getOrElse(-1),
          reason
        ))
      case Array("BYE", scores @ _*) =>
        Some(Bye(scores))
      case Array("MOVE", "PASS") =>
        Some(Pass)
      case Array("MOVE", m) =>
        if(m.length == 2 && 'A' <= m(0) && m(0) <= 'H' && '1' <= m(1) && m(1) <= '8')
          Some(Move((m(0) - 'A') * 8 + (m(1) - '1')))
        else {
          Console.err.println(s"unknown move: '$m'")
          None
        }
      case Array("ACK", time) =>
        Some(Ack(Try(time.toInt).getOrElse(-1)))
      case _ =>
        Console.err.println(s"unknown command: '$str'")
        None
    }
  }
}
