package net.akouryy.kreins
package model

import util.BitUtil

final case class Board(fst: Panel, snd: Panel) {
  def pass = Board(snd, fst)

  def countBlack = BitUtil.popcount(fst.code)

  def countWhite = BitUtil.popcount(snd.code)

  def countEmpty = BitUtil.popcount(~(fst | snd).code)

  @inline def &(c: Long) = Board(fst & c, snd & c)

  @inline def |(c: Long) = Board(fst | c, snd | c)

  def mirrorWithDiagRightDown =
    Board(fst.mirrorWithDiagRightDown, snd.mirrorWithDiagRightDown)

  def mirrorWithDiagRightUp =
    Board(fst.mirrorWithDiagRightUp, snd.mirrorWithDiagRightUp)

  def mirrorWithHorizontal =
    Board(fst.mirrorWithHorizontal, snd.mirrorWithHorizontal)

  def mirrorWithVertical =
    Board(fst.mirrorWithVertical, snd.mirrorWithVertical)

  def rotate45ACW = Board(fst.rotate45ACW, snd.rotate45ACW)

  def rotate45CW = Board(fst.rotate45CW, snd.rotate45CW)

  /**
    * https://primenumber.hatenadiary.jp/entry/2016/12/26/063226
    */
  @inline def possPlaceableToLeft = {
    val a1 = fst.code & BitUtil.ROWS_0246
    val b1 = snd.code & BitUtil.ROWS_0246
    val c1 = a1 << 1
    val d1 = ~(a1 | b1 | c1) & (b1 + c1) & BitUtil.ROWS_0246

    val a2 = fst.code & BitUtil.ROWS_1357
    val b2 = snd.code & BitUtil.ROWS_1357
    val c2 = a2 << 1
    val d2 = ~(a2 | b2 | c2) & (b2 + c2) & BitUtil.ROWS_1357

    Panel(d1 | d2)
  }

  @inline def possPlaceableToLeftOrRight =
    possPlaceableToLeft |
      mirrorWithVertical.possPlaceableToLeft.mirrorWithVertical

  /**
    * https://primenumber.hatenadiary.jp/entry/2016/12/26/063226
    */
  @inline def possPlaceable = {
    val a = rotate45ACW
    val c = rotate45CW
    possPlaceableToLeftOrRight |
      mirrorWithDiagRightUp.possPlaceableToLeftOrRight.mirrorWithDiagRightUp | (
      (
        (a & BitUtil.ROT45ACW_MASK).possPlaceableToLeftOrRight & BitUtil.ROT45ACW_MASK |
          (a & ~BitUtil.ROT45ACW_MASK).possPlaceableToLeftOrRight & ~BitUtil.ROT45ACW_MASK
        ).rotate45CW |
        /* inverse of `rotate45ACW` is `moveRowUp . rotate45CW` */
        (
          (c & BitUtil.ROT45CW_MASK).possPlaceableToLeftOrRight & BitUtil.ROT45CW_MASK |
            (c & ~BitUtil.ROT45CW_MASK).possPlaceableToLeftOrRight & ~BitUtil.ROT45CW_MASK
          ).rotate45ACW
      ).moveRowUp
  }

  /**
    * https://primenumber.hatenadiary.jp/entry/2016/12/26/063226
    * http://www.amy.hi-ho.ne.jp/okuhara/flipcuda.htm
    *
    * @param pos 石が置かれた座標 (0 to 63)
    */
  def possToFlip(pos: Int) = {
    val omx = snd.code
    val omYZW = snd.code & 0x7e7e7e7e7e7e7e7eL
    var f = 0L;
    {
      val maskX = 0x0080808080808080L >>> 63 - pos
      val maskY = 0x7F00000000000000L >>> 63 - pos
      val maskZ = 0x0102040810204000L >>> 63 - pos
      val maskW = 0x0040201008040201L >>> 63 - pos
      val ofX = fst.code & BitUtil.firstHighBit(maskX & ~omx)
      val ofY = fst.code & BitUtil.firstHighBit(maskY & ~omYZW)
      val ofZ = fst.code & BitUtil.firstHighBit(maskZ & ~omYZW)
      val ofW = fst.code & BitUtil.firstHighBit(maskW & ~omYZW)
      f |= (-ofX << 1) & maskX
      f |= (-ofY << 1) & maskY
      f |= (-ofZ << 1) & maskZ
      f |= (-ofW << 1) & maskW
    }
    {
      val maskX = 0x0101010101010100L << pos
      val maskY = 0x00000000000000feL << pos
      val maskZ = 0x0002040810204080L << pos
      val maskW = 0x8040201008040200L << pos
      val ofX = maskX & ((omx | ~maskX) + 1) & fst.code
      val ofY = maskY & ((omYZW | ~maskY) + 1) & fst.code
      val ofZ = maskZ & ((omYZW | ~maskZ) + 1) & fst.code
      val ofW = maskW & ((omYZW | ~maskW) + 1) & fst.code
      f |= ofX - ((ofX | ~(ofX - 1)) >>> 63) & maskX
      f |= ofY - ((ofY | ~(ofY - 1)) >>> 63) & maskY
      f |= ofZ - ((ofZ | ~(ofZ - 1)) >>> 63) & maskZ
      f |= ofW - ((ofW | ~(ofW - 1)) >>> 63) & maskW
    }
    Panel(f)
  }

  def place(stone: Int, flipped: Panel) =
    Board(snd & ~flipped.code, fst | flipped | (1L << stone))

  override def toString = {
    val fm = fst.toMatrix
    val sm = snd.toMatrix

    s"Board(${fst.code}, ${snd.code}):\n+---------------+\n" +
      (fm zip sm).map { case (fr, sr) =>
        "|" +
          (fr zip sr).map { case (f, s) =>
            if(f) if(s) '!' else 'X' else if(s) 'O' else ' '
          }.mkString(" ") +
          "|"
      }.mkString("\n") +
      "\n+---------------+"
  }
}

object Board {
  val initialBoard = Board(
    Panel(0x0000000810000000L),
    Panel(0x0000001008000000L)
  )

  def fromMatrix(f: Seq[Seq[Boolean]], s: Seq[Seq[Boolean]]) = {
    Board(Panel.fromMatrix(f), Panel.fromMatrix(s))
  }

  def fromScrLine(s: String) =
    Board(
      Panel(s.reverseMap(c => if(c == 'O') 1L else 0L).reduce(_ << 1 | _)),
      Panel(s.reverseMap(c => if(c == 'X') 1L else 0L).reduce(_ << 1 | _))
    )
}
