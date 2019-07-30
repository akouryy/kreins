package net.akouryy.kreins
package study

import game.{Board, Log}
import strategy.Pattern

class PatternWeight(val fromStone: Int, val untilStone: Int) {

  import Pattern.patterns

  val blacks = Array.ofDim[Array[Int]](patterns.length)
  val whites = Array.ofDim[Array[Int]](patterns.length)

  for(i <- patterns.indices) {
    blacks(i) = Array.ofDim(patterns(i).codeLimit)
    whites(i) = Array.ofDim(patterns(i).codeLimit)
  }

  def load(l: Log) {
    l.seq.zipWithIndex.foreach { case ((bd, _), h) =>
      for(
        (f, b0) <- Seq((true, bd), (false, bd.pass))
        if fromStone <= h && h < untilStone;
        b1 <- Seq(b0, b0.mirrorWithHorizontal);
        b2 <- Seq(b1, b1.mirrorWithVertical);
        b <- Seq(b2, b2.mirrorWithDiagRightDown);
        i <- patterns.indices
      ) {
        val p = patterns(i).code(b)
        (if(f == (h % 2 == 0)) l.finalResult else ~l.finalResult) match {
          case Board.FstWin =>
            blacks(i)(p) += 2
          case Board.SndWin =>
            whites(i)(p) += 2
          case Board.Draw =>
            blacks(i)(p) += 1
            whites(i)(p) += 1
          case Board.NotEnd =>
        }
      }
    }
  }

  def weight = {
    blacks zip whites map { case (bs, ws) =>
      bs zip ws map { case (b, w) =>
        (120 * (b - w) / (b + w).max(10)).toByte
      }
    }
  }
}
