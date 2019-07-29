package net.akouryy.kreins
package strategy

import game.{Board, LightBoard, Pos}
import encoder.PlacementTableEncoder
import util.ConsoleUtil.Ansi

import scala.util.Random

class DyagsekiSearch(val pt: PlacementTableEncoder.PlacementTable) {
  def bestMove(board: Board): Option[Int] = {
    val pp = board.possPlaceable.code

    if(pp == 0) {
      Some(-1)
    } else {
      for(
        (b, pFn) <- Seq[(LightBoard, Byte => Byte)](
          (board.toLightBoard, p => p),
          (board.mirrorWithVertical.mirrorWithHorizontal.toLightBoard, Pos.rotate180),
          (board.mirrorWithDiagRightUp.toLightBoard, Pos.mirrorWithDiagRightUp),
          (board.mirrorWithDiagRightDown.toLightBoard, Pos.mirrorWithDiagRightDown)
        )
      ) {
        for(ls <- pt.get(b)) {
          var rand = Random.nextInt(ls.map(_._2.toInt).sum)
          for((p0, _) <- ls.find { case (_, n) =>
            rand -= n
            rand <= 0
          }) {
            if(Kreins.isDebug) println(Ansi.bSky(s"dys: $ls"))
            return Some(pFn(p0).toInt)
          }
          if(Kreins.isDebug) println(Ansi.bRed(s"BUG: dys returns None for $ls"))
        }
      }
      None
    }
  }
}

