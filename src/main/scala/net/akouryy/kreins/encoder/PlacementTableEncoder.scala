package net.akouryy.kreins
package encoder

import java.io.{InputStream, OutputStream}
import scala.collection.mutable

import game.LightBoard

object PlacementTableEncoder {

  type PlacementTable = Map[LightBoard, List[(Byte, Short)]]

  type CompactPTable = (PlacementTable, Map[LightBoard, Byte])

  val HEADER = 'd' << 24 | 'y' << 16 | 's' << 8 | 0

  def encode(o: OutputStream, pt: PlacementTable) {
    BytesEncoder.encodeOne(o, 4, HEADER.toLong)

    for((b, ss) <- pt) {
      BytesEncoder.encodeOne(o, 8, b.fst)
      BytesEncoder.encodeOne(o, 8, b.snd)

      ss match {
        case List((pos, _)) =>
          BytesEncoder.encodeOne(o, 1, (64 | pos).toLong)
        case _ =>
          var rest = ss.size
          for((pos, score) <- ss) {
            rest -= 1
            val isLastFlag = if(rest == 0) 64 else 0
            BytesEncoder.encodeOne(o, 1, (isLastFlag | pos).toLong)
            BytesEncoder.encodeOne(o, 1, score.toLong)
          }
      }
    }
  }

  def decode(i: InputStream): CompactPTable = {
    val ret1 =
      mutable.Map[LightBoard, List[(Byte, Short)]]()

    val ret2 =
      mutable.Map[LightBoard, Byte]()

    if(!BytesEncoder.decodeOne(i, 4).contains(HEADER.toLong)) {
      throw new Error("Invalid dys format or version.")
    }

    var fstCodeOpt: Option[Long] = None
    while( {
      fstCodeOpt = BytesEncoder.decodeOne(i, 8)
      fstCodeOpt.nonEmpty
    }) {
      val Some(fstCode) = fstCodeOpt
      val Some(sndCode) = BytesEncoder.decodeOne(i, 8)
      var ss = List[(Byte, Short)]()
      locally {
        var pos = -1.toShort
        var isLast = false
        while(!isLast) {
          pos = BytesEncoder.decodeOne(i, 1).get.toShort
          isLast = (pos & 64) == 64
          val score =
            if(ss.isEmpty && isLast) {
              255 // only element
            } else {
              BytesEncoder.decodeOne(i, 1).get
            }
          ss ::= (((pos & ~64).toByte, score.toShort))
        }
      }

      ss match {
        case List((pos, _)) =>
          ret2(new LightBoard(fstCode, sndCode)) = pos
        case _ =>
          ret1(new LightBoard(fstCode, sndCode)) = ss
      }
    }

    (ret1.toMap, ret2.toMap)
  }
}
