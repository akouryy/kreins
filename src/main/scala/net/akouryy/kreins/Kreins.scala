package net.akouryy.kreins

import game.Game
import player.Player

object Kreins extends App {
  def readInt() = scala.io.StdIn.readInt()

  val pgs = Player.generators

  println(s"players: ${
    pgs.zipWithIndex.map { case (pg, i) => s"$i: ${pg.nickname}" }.mkString(", ")
  }")
  print(s"Alice [0-${pgs.length - 1}]: ")
  val aliceGen = pgs(readInt())
  val alice = aliceGen.fromStdin()
  print(s"Bob [0-${pgs.length - 1}]: ")
  val bobGen = pgs(readInt())
  val bob = bobGen.fromStdin()
  print(s"Game count: ")
  val nGames = readInt()

  if(nGames <= 0) {
    val g = Game(alice, bob)
    g.run()
    println(s"${g.blackBoard.result}\n${g.blackBoard}")
  } else {
    print(s"Show statistics for every 2n games. n: ")
    val nShow = readInt()

    var afWin, asWin, bfWin, bsWin, draw = 0
    for(t <- 0 until nGames) {
      import game.Board._
      val aliceFirst = t % 2 == 0
      val g = if(aliceFirst) Game(alice, bob) else Game(bob, alice)
      g.run()
      g.blackBoard.result match {
        case FstWin => if(aliceFirst) afWin += 1 else bfWin += 1
        case SndWin => if(aliceFirst) bsWin += 1 else asWin += 1
        case Draw => draw += 1
        case NotEnd => throw new RuntimeException(s"game not end\n${g.blackBoard}")
      }

      if((t / 2) % nShow == nShow - 1) {
        println(f"[$aliceFirst%5s] Alice ${
          if(aliceFirst) afWin else asWin
        }%5d, Bob ${
          if(aliceFirst) bsWin else bfWin
        }%5d, draw $draw")
      }
    }

    println(
      f"\ntype:   Alice: ${aliceGen.nickname}%-30s / Bob: ${bobGen.nickname}%-30s\n" +
        f"first:  Alice: $afWin%30d / Bob: $bfWin%30d\n" +
        f"second: Alice: $asWin%30d / Bob: $bsWin%30d\n" +
        f"total:  Alice: ${afWin + asWin}%30d / Bob: ${bfWin + bsWin}%30d"
    )
  }
}
