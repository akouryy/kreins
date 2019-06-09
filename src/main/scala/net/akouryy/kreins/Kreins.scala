package net.akouryy.kreins

object Kreins extends App {
  println("Kreins started.")

  println(model.Board.initialBoard)
  println(model.Board.initialBoard.pass)
  println(util.BitUtil.popcount(model.Board.initialBoard.fst.code))
  println(util.BitUtil.popcount(model.Board.initialBoard.snd.code))
}
