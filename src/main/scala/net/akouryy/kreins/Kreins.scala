package net.akouryy.kreins

import fl.Client
import game.Game
import player.Player
import scopt.OParser
import util.InputUtil

object Kreins {

  final case class CmdConfig(
    host: String = "localhost",
    port: Int = 8000,
    playerName: String = "kreins　v0.1.1"
  )

  val cmdParser = {
    val builder = OParser.builder[CmdConfig]
    import builder._
    OParser.sequence(
      programName("kreins"),
      head("kreins", "0.1"),
      opt[String]('H', "host")
        .action((x, c) => c.copy(host = x))
        .text("host name"),
      opt[Int]('p', "port")
        .action((x, c) => c.copy(port = x))
        .text("port number"),
      opt[String]('n', "name")
        .action((x, c) => c.copy(playerName = x))
        .text("player name")
    )
  }

  def main(args: Array[String]) = {
    if(args.length == 0)
      interactive()
    else {
      OParser.parse(cmdParser, args, CmdConfig()) match {
        case Some(CmdConfig(host, port, name)) =>
          new Client(host, port, name).run()
        case None => // exit
      }
    }
  }

  def interactive() = {
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

    val allowResign =
      InputUtil.readInt("Allow resign [0-1](1): ").forall(_ == 1)
    val nGames =
      InputUtil.readInt("Game count [0-](1000): ").filter(_ >= 0).getOrElse(1000)

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
        alice.reset()
        bob.reset()
      }

      println(
        f"\ntype:   Alice: ${aliceGen.nickname}%-30s / Bob: ${bobGen.nickname}%-30s\n" +
          f"first:  Alice: $afWin%30d / Bob: $bfWin%30d\n" +
          f"second: Alice: $asWin%30d / Bob: $bsWin%30d\n" +
          f"total:  Alice: ${afWin + asWin}%30d / Bob: ${bfWin + bsWin}%30d"
      )
    }
  }
}
