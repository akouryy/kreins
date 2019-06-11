package net.akouryy.kreins
package game

import scala.collection.mutable

case class Log(seq: Seq[(Board, Option[Int])]) {
  override def toString = s"Log(..., ${seq.last})"

  val finalResult = {
    val b = seq.last._1
    if(seq.length % 2 == 1)
      b.result
    else
      ~b.result
  }
}

object Log {
  def fromMoves(moves: Seq[(Boolean, Int)]) = {
    var b = Board.InitialBoard
    var isBlackFirst = true
    val seq =
      mutable.ListBuffer[(Board, Option[Int])]()

    for((isBlack, stone) <- moves) {
      if(isBlack != isBlackFirst) {
        seq += ((b, None))
        b = b.pass
      }

      seq += ((b, Some(stone)))
      b = b.place(stone)
      isBlackFirst = !isBlack
    }
    seq += ((b, None))

    Log(seq)
  }
}
