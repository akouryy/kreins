package net.akouryy.kreins
package encoder

import java.io.{InputStream, ObjectInputStream, ObjectOutputStream, OutputStream}

import game.Board

object PlacementTableEncoder {

  case class PlacementTable(pt: Map[Board, List[(Byte, Byte)]])

  def encode(o: OutputStream, pt: PlacementTable) {
    val oo = new ObjectOutputStream(o)
    oo.writeObject(pt)
  }

  def decode(i: InputStream) = {
    val oi = new ObjectInputStream(i)
    oi.readObject().asInstanceOf[PlacementTable]
  }
}
