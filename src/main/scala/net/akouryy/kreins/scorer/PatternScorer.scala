package net.akouryy.kreins
package scorer

import java.io.{FileInputStream, FileOutputStream}

import encoder.PatternWeightEncoder
import game.Board
import strategy.Pattern
import util.{InputUtil, Loan}

class PatternScorer(file: String) extends Scorer {
  lazy val weight =
    Loan(new FileInputStream(file))(PatternWeightEncoder.decode)

  def score(board: Board) = {
    import Pattern.patterns
    var sc = 0
    for(
      b1 <- Seq(board, board.mirrorWithHorizontal);
      b2 <- Seq(b1, b1.mirrorWithVertical);
      b <- Seq(b2, b2.mirrorWithDiagRightDown);
      i <- patterns.indices
    ) {
      sc += weight(i)(patterns(i).code(b))
    }

    sc
  }
}

object PatternScorer {
  def fromStdin() = {
    val wgtFile = InputUtil.readLineWithRetry("wgt file: ")
    new scorer.PatternScorer(wgtFile)
  }
}
