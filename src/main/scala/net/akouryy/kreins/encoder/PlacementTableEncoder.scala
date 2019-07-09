package net.akouryy.kreins
package encoder

import java.io.{InputStream, ObjectInputStream, ObjectOutputStream, OutputStream}

import game.{Board, Panel}

import scala.collection.mutable

object PlacementTableEncoder {

  type PlacementTable = Map[Board, List[(Byte, Int)]]

  def encode(o: OutputStream, pt: PlacementTable) {
    for((b, ss) <- pt) {
      BytesEncoder.encodeOne(o, 8, b.fst.code)
      BytesEncoder.encodeOne(o, 8, b.snd.code)
      BytesEncoder.encodeOne(o, 1, ss.size)
      for((pos, score) <- ss) {
        BytesEncoder.encodeOne(o, 1, pos)
        BytesEncoder.encodeOne(o, 1, score)
      }
    }
  }

  def decode(i: InputStream): PlacementTable = {
    val ret =
      mutable.Map[Board, List[(Byte, Int)]]()

    var fstCodeOpt: Option[Long] = None
    while( {
      fstCodeOpt = BytesEncoder.decodeOne(i, 8)
      fstCodeOpt.nonEmpty
    }) {
      val Some(fstCode) = fstCodeOpt
      val Some(sndCode) = BytesEncoder.decodeOne(i, 8)
      val Some(ssLen) = BytesEncoder.decodeOne(i, 1)
      val ss = 0.until(ssLen.toInt).map { _ =>
        val Some(pos) = BytesEncoder.decodeOne(i, 1)
        val Some(score) = BytesEncoder.decodeOne(i, 1)
        (pos.toByte, score.toInt)
      }.toList

      ret(Board(Panel(fstCode), Panel(sndCode))) = ss
    }

    ret.toMap
  }
}
