package net.akouryy.kreins

import fl.Client
import net.akouryy.kreins.action.GamStudyDyagsekiAction
import scopt.OParser

object Kreins {

  val VERSION = "0.4.1"

  sealed trait SubCommand

  case object SubCommandRun extends SubCommand

  case object SubCommandDyagseki extends SubCommand

  final case class CmdConfig(
    host: String = "localhost",
    isDebug: Boolean = false,
    port: Int = 8000,
    playerName: String = s"kreins-$VERSION",
    dysGzFile: String = "",
    subCommand: Option[SubCommand] = None
  )

  var isDebug = false

  val cmdParser = {
    val builder = OParser.builder[CmdConfig]
    import builder._
    OParser.sequence(
      programName("kreins"),
      head("kreins", VERSION),
      cmd("run")
        .action((_, c) => c.copy(subCommand = Some(SubCommandRun)))
        .text("  Run a reversi client.")
        .children(
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
        ),

      cmd("dys")
        .action((_, c) => c.copy(subCommand = Some(SubCommandDyagseki)))
        .text("  Convert .gam file to .dys.gz"),
      checkConfig {
        case CmdConfig(_, _, _, _, _, None) => failure("Specify a command.")
        case _ => success
      }
    )
  }

  def main(args: Array[String]) = {
    OParser.parse(cmdParser, args, CmdConfig()) match {
      case Some(CmdConfig(host, isDbg, port, name, dys, Some(SubCommandRun))) =>
        isDebug = isDbg
        new Client(host, port, name, dys).run()
      case Some(CmdConfig(_, _, _, _, _, Some(SubCommandDyagseki))) =>
        GamStudyDyagsekiAction.run()
      case Some(CmdConfig(_, _, _, _, _, None)) => // exit
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
