package net.akouryy.kreins
package action

import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream

import encoder.PlacementTableEncoder
import game.LightBoard
import strategy.DyagsekiSearch
import study.parser.GamParser
import util.{InputUtil, Loan}

import scala.collection.mutable
import scala.io.Source

object GamStudyDyagsekiAction {
  def run() {
    val gamFileName = InputUtil.readLineWithRetry("gam file name: ").trim
    val wgtFileName = InputUtil.readLineWithRetry("dys.gz file name: ").trim

    val placements =
      mutable.Map[LightBoard, mutable.Map[Byte, Int]]()

    Loan(Source.fromFile(gamFileName)) { sc =>
      sc.getLines.foreach { l =>
        GamParser.parse(l) match {
          case Left(s) => println(s)
          case Right(lg) =>
            for((b, Some(p)) <- lg.seq.take(DyagsekiSearch.MaxTurn)) {
              val ps =
                placements.getOrElseUpdate(b.toLightBoard, mutable.Map[Byte, Int]())
              ps(p.toByte) = ps.getOrElse(p.toByte, 0) + 1
            }
        }
      }
    }

    val pt = placements.mapValues { ps =>
      val l = ps.toList
      val max = l.map(_._2).max
      l.map { case (p, n) => (p, (n * 255 / max).toShort) }
    }.toMap /* convert to immutable Map */

    Loan(new GZIPOutputStream(new FileOutputStream(wgtFileName))) { o =>
      PlacementTableEncoder.encode(o, pt)
    }
  }
}
