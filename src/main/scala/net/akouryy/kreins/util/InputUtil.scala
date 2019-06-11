package net.akouryy.kreins.util

import scala.io.StdIn

object InputUtil {
  def readInt(prompt: String = "") = try {
    print(prompt)
    Some(StdIn.readInt())
  } catch {
    case _: NumberFormatException => None
  }

  def readIntWithRetry(prompt: String = "", cond: Int => Boolean = _ => true): Int = {
    while(true) {
      readInt(prompt).filter(cond) match {
        case Some(i) => return i
        case _ =>
      }
    }
    -1
  }

  def readLineWithRetry(prompt: String = "", cond: String => Boolean = _ => true): String = {
    while(true) {
      val s = StdIn.readLine("%s", prompt)
      if(s == null) return ""
      if(cond(s)) return s
    }
    ""
  }
}
