package net.akouryy.kreins
package util

object ConsoleUtil {

  object Ansi {
    val bBlackStart = "\u001b[48;5;16m"
    val fBlackStart = "\u001b[38;5;16m"
    val reset = s"\u001b[0m$bBlackStart"

    val bOrangeStart = s"\u001b[48;5;220m$fBlackStart"
    val fOrangeStart = s"\u001b[38;5;220m$bBlackStart"
    val fSkyStart = s"\u001b[38;5;51m$bBlackStart"

    def bOrange(s: String) = bOrangeStart + s + reset

    def fOrange(s: String) = fOrangeStart + s + reset

    def fSky(s: String) = fSkyStart + s + reset
  }

}
