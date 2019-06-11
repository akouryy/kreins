package net.akouryy.kreins

import game.Game
import net.akouryy.kreins.util.InputUtil
import player.Player

object Kreins extends App {
  val pgs = Player.generators

  println(s"players:\n${
    pgs.zipWithIndex.map { case (pg, i) => s"  $i: ${pg.nickname}" }.mkString("\n")
  }")
  val aliceGen = pgs(InputUtil.readIntWithRetry(
    s"Alice [0-${pgs.length - 1}]: ",
    pgs.indices.contains
  ))
  val alice = aliceGen.fromStdin()

  val bobGen = pgs(InputUtil.readIntWithRetry(
    s"Bob [0-${pgs.length - 1}]: ",
    pgs.indices.contains
  ))
  val bob = bobGen.fromStdin()

  val nGames = InputUtil.readIntWithRetry("Game count [0-]: ", _ >= 0)
  val allowResign =
    InputUtil.readInt("Allow resign [0-1](1): ").forall(_ == 1)

  if(nGames == 0) {
    val g = Game(alice, bob)
    g.run(allowResign)
    println(s"${g.blackBoard.result}\n${g.blackBoard}")
  } else {
    val nShow = InputUtil.readIntWithRetry("Show statistics for every 2n games. n: ")

    var afWin, asWin, bfWin, bsWin, draw = 0
    for(t <- 0 until nGames) {
      import game.Board._
      val aliceFirst = t % 2 == 0
      val g = if(aliceFirst) Game(alice, bob) else Game(bob, alice)
      g.run(allowResign)
      (g.blackBoard.result, g.resignedBy) match {
        case (FstWin(_), _) | (_, Some(false)) => if(aliceFirst) afWin += 1 else bfWin += 1
        case (SndWin(_), _) | (_, Some(true)) => if(aliceFirst) bsWin += 1 else asWin += 1
        case (Draw, _) => draw += 1
        case (NotEnd, _) =>
          throw new RuntimeException(s"game not end\n${g.blackBoard}")
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
