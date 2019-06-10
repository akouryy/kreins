package net.akouryy.kreins.util

import scala.io.StdIn

object InputUtil {
  def readInt() = try {
    Some(StdIn.readInt())
  } catch {
    case _: NumberFormatException => None
  }
}
