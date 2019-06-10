package net.akouryy.kreins

import game.Game
import player.Player

object Kreins extends App {
  def readInt() = scala.io.StdIn.readInt()

  val pgs = Player.generators

  println(s"players: ${
    pgs.zipWithIndex.map { case (pg, i) => s"$i: ${pg.nickname}" }.mkString(", ")
  }")
  print(s"Black [0-${pgs.length - 1}]: ")
  val black = pgs(readInt()).fromStdin()
  print(s"White [0-${pgs.length - 1}]: ")
  val white = pgs(readInt()).fromStdin()
  print(s"Game count: ")
  val nGames = readInt()

  if(nGames <= 1) {
    val g = Game(black, white)
    g.run()
    println(s"${g.blackBoard.result}\n${g.blackBoard}")
  } else {
    var bWin, wWin, draw = 0
    for(t <- 0 until nGames) {
      import game.Board._
      val g = Game(black, white)
      g.run()
      g.blackBoard.result match {
        case FstWin => bWin += 1
        case SndWin => wWin += 1
        case Draw => draw += 1
        case NotEnd => throw new RuntimeException(s"game not end\n${g.blackBoard}")
      }

      if(t / 16 == 15) {
        println(s"black ${bWin}, white ${wWin}, draw ${draw}")
      }
    }

    println(s"black ${bWin}, white ${wWin}, draw ${draw}")
  }
}
