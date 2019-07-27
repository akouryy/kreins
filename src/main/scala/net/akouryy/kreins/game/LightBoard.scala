package net.akouryy.kreins
package game

final class LightBoard(val fst: Long, val snd: Long) {
  @inline override def equals(that: Any) = that match {
    case b: LightBoard => fst == b.fst && snd == b.snd
    case _ => false
  }

  @inline override def hashCode = {
    ((fst.toInt * 31 + (fst >>> 32).toInt) * 31 + snd.toInt) * 31 + (snd >>> 32).toInt
  }
}
