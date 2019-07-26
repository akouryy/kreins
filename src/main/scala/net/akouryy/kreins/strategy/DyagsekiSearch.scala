package net.akouryy.kreins
package strategy

import game.{Board, Pos}

import scala.util.Random

class DyagsekiSearch(val pt: Map[Board, List[(Byte, Int)]]) {
  def bestMove(board: Board): Option[Int] = {
    val pp = board.possPlaceable.code

    if(pp == 0) {
      Some(-1)
    } else {
      for(
        (b, pFn) <- Seq[(Board, Byte => Byte)](
          (board, p => p),
          (board.mirrorWithVertical.mirrorWithHorizontal, Pos.rotate180),
          (board.mirrorWithDiagRightUp, Pos.mirrorWithDiagRightUp),
          (board.mirrorWithDiagRightDown, Pos.mirrorWithDiagRightDown)
        )
      ) {
        for(ls <- pt.get(b)) {
          var rand = Random.nextInt(ls.map(_._2).sum)
          for((p0, _) <- ls.find { case (_, n) =>
            rand -= n
            rand <= 0
          }) {
            println(ls)
            return Some(pFn(p0).toInt)
          }
        }
      }
      None
    }
  }
}

