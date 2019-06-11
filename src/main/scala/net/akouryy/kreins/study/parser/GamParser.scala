package net.akouryy.kreins
package study.parser

import game.Board
import game.Log
import util.Loan

import scala.io.Source

object GamParser {
  def parse(s: String): Either[String, Log] = {
    val l = s.trim
    if(l.isEmpty) {
      Left("empty line")
    } else {
      val ls = l.split(" ")
      if(ls.length != 3) {
        return Left(s"line did not have 3 columns: $l")
      }

      val log =
        Log.fromMoves(ls(0).slice(0, ls(0).length - 1).grouped(3).map { m =>
          (m(0) == '+', m(1) - 'a' << 3 | m(2) - '1')
        }.toSeq)

      val diff = ls(1).toInt
      val result =
        if(diff > 0) Board.FstWin(diff)
        else if(diff == 0) Board.Draw
        else Board.SndWin(-diff)

      if(log.finalResult != result) {
        return Left(
          s"line result is not correct: \nline: $l\nlog: $result ${log.seq.last}"
        )
      }
      Right(log)
    }
  }
}
