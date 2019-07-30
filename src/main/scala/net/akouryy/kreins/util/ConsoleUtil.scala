package net.akouryy.kreins
package util

object ConsoleUtil {

  object Ansi {
    val bBlackStart = "\u001b[48;5;16m"
    val fBlackStart = "\u001b[38;5;16m"
    val fWhiteStart = "\u001b[38;5;231m"
    val reset = s"\u001b[0m$bBlackStart"

    val bOrangeStart = s"\u001b[48;5;220m$fBlackStart"
    val fOrangeStart = s"\u001b[38;5;220m$bBlackStart"
    val bRedStart = s"\u001b[48;5;124m$fWhiteStart"
    val fRedStart = s"\u001b[38;5;124m$bBlackStart"
    val bSkyStart = s"\u001b[48;5;51m$fBlackStart"
    val fSkyStart = s"\u001b[38;5;51m$bBlackStart"

    def bOrange(s: String) = bOrangeStart + s + reset

    def fOrange(s: String) = fOrangeStart + s + reset

    def bRed(s: String) = bRedStart + s + reset

    def fRed(s: String) = fRedStart + s + reset

    def bSky(s: String) = bSkyStart + s + reset

    def fSky(s: String) = fSkyStart + s + reset
  }

}
