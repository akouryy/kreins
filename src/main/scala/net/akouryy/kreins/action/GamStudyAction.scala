package net.akouryy.kreins.action

import java.io.FileOutputStream

import net.akouryy.kreins.encoder.PatternWeightEncoder
import net.akouryy.kreins.study.PatternWeight
import net.akouryy.kreins.study.parser.GamParser
import net.akouryy.kreins.util.{InputUtil, Loan}

import scala.io.Source

object GamStudyAction {
  def main(args: Array[String]) {
    val gamFileName = InputUtil.readLineWithRetry("gam file name: ").trim
    val wgtFileName = InputUtil.readLineWithRetry("wgt file name: ").trim

    val pw = new PatternWeight(0, 50)

    var i = 0
    Loan(Source.fromFile(gamFileName)) { sc =>
      sc.getLines.foreach { l =>
        GamParser.parse(l) match {
          case Right(l) => pw.load(l)
          case Left(s) => println(s)
        }
        i += 1
        if((i & 511) == 0) {
          println(i)
        }
      }
    }

    Loan(new FileOutputStream(wgtFileName)) { f =>
      PatternWeightEncoder.encode(f, pw.weight)
    }
  }
}
