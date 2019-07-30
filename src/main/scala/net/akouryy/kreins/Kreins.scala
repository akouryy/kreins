package net.akouryy.kreins

import fl.Client
import scopt.OParser

object Kreins {

  val VERSION = "0.4.1"

  final case class CmdConfig(
    host: String = "localhost",
    isDebug: Boolean = false,
    port: Int = 8000,
    playerName: String = s"kreins-$VERSION",
    dysGzFile: String = ""
  )

  var isDebug = false

  val cmdParser = {
    val builder = OParser.builder[CmdConfig]
    import builder._
    OParser.sequence(
      programName("kreins"),
      head("kreins", VERSION),
      opt[String]('H', "host")
        .action((x, c) => c.copy(host = x))
        .text("[required] host name"),
      opt[Int]('p', "port")
        .action((x, c) => c.copy(port = x))
        .text("[required] port number"),
      opt[String]('n', "name")
        .action((x, c) => c.copy(playerName = x))
        .text("[required] player name"),
      opt[String]('z', "dys")
        .required
        .action((x, c) => c.copy(dysGzFile = x))
        .text("[required] .dys.gz file name"),
      opt[Unit]('D', "debug")
        .action((_, c) => c.copy(isDebug = true))
        .text("Enable debug output (slow)")
    )
  }

  def main(args: Array[String]) = {
    OParser.parse(cmdParser, args, CmdConfig()) match {
      case Some(CmdConfig(host, isDbg, port, name, dys)) =>
        isDebug = isDbg
        new Client(host, port, name, dys).run()
      case None => // exit
    }
  }

  def gc1Sec() = {
    System.gc()
    Thread.sleep(500)
    System.gc()
    Thread.sleep(500)
    System.gc()
  }
}
