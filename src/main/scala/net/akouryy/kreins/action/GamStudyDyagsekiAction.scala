package net.akouryy.kreins.action

import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream

import net.akouryy.kreins.encoder.PlacementTableEncoder
import net.akouryy.kreins.game.Board
import net.akouryy.kreins.study.parser.GamParser
import net.akouryy.kreins.util.{InputUtil, Loan}

import scala.collection.mutable
import scala.io.Source

object GamStudyDyagsekiAction {
  def _main(args: Array[String]) {
    val gamFileName = InputUtil.readLineWithRetry("gam file name: ").trim
    val wgtFileName = InputUtil.readLineWithRetry("dys.gz file name: ").trim

    val placements =
      mutable.Map[Board, mutable.Map[Byte, Int]]()

    Loan(Source.fromFile(gamFileName)) { sc =>
      sc.getLines.foreach { l =>
        GamParser.parse(l) match {
          case Left(s) => println(s)
          case Right(lg) =>
            for((b, Some(p)) <- lg.seq.take(30)) {
              val ps =
                placements.getOrElseUpdate(b, mutable.Map[Byte, Int]())
              ps(p.toByte) = ps.getOrElse(p.toByte, 0) + 1
            }
        }
      }
    }

    val pt = placements.mapValues { ps =>
      val l = ps.toList
      val max = l.map(_._2).max
      l.map { case (p, n) => (p, n * 255 / max) }
    }.toMap /* convert to immutable Map */

    Loan(new GZIPOutputStream(new FileOutputStream(wgtFileName))) { o =>
      PlacementTableEncoder.encode(o, pt)
    }
  }
}
