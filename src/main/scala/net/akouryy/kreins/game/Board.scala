package net.akouryy.kreins
package game

import net.akouryy.kreins.util.ConsoleUtil.Ansi
import util.BitUtil

final class Board(
  val fst: Panel,
  val snd: Panel,
  private val _pass: Board,
  val countEmpty: Int
) {

  import Board._

  lazy val pass = if(_pass != null) _pass else Board(snd, fst, this, countEmpty)

  lazy val countFst = BitUtil.popcount(fst.code)

  lazy val countSnd = BitUtil.popcount(snd.code)

  lazy val isEnd =
    countEmpty == 0 ||
      possPlaceable.code == 0 && pass.possPlaceable.code == 0

  lazy val result =
    if(isEnd) {
      val diff = countFst - countSnd
      if(diff > 0) FstWin(diff)
      else if(diff == 0) Draw
      else SndWin(-diff)
    } else {
      NotEnd
    }

  @inline def isEnd1 =
    countEmpty == 1 ||
      possPlaceable.code == 0 && pass.possPlaceable.code == 0

  @inline def result1 =
    if(isEnd) {
      result
    } else if(countEmpty == 1) {
      if(possPlaceable.code != 0) {
        ~place(BitUtil.firstHighBitPos(possPlaceable.code)).result
      } else if(pass.possPlaceable.code != 0) {
        pass.place(BitUtil.firstHighBitPos(pass.possPlaceable.code)).result
      } else {
        // must not reach here
        result
      }
    } else {
      NotEnd
    }

  @inline def &(c: Long) = Board(fst & c, snd & c, null, -1)

  @inline def |(c: Long) = Board(fst | c, snd | c, null, -1)

  @inline def mirrorWithDiagRightDown =
    Board(fst.mirrorWithDiagRightDown, snd.mirrorWithDiagRightDown, null, countEmpty)

  @inline def mirrorWithDiagRightUp =
    Board(fst.mirrorWithDiagRightUp, snd.mirrorWithDiagRightUp, null, countEmpty)

  @inline def mirrorWithHorizontal =
    Board(fst.mirrorWithHorizontal, snd.mirrorWithHorizontal, null, countEmpty)

  @inline def mirrorWithVertical =
    Board(fst.mirrorWithVertical, snd.mirrorWithVertical, null, countEmpty)

  @inline def rotate45ACW = Board(fst.rotate45ACW, snd.rotate45ACW, null, countEmpty)

  @inline def rotate45CW = Board(fst.rotate45CW, snd.rotate45CW, null, countEmpty)

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
  @inline lazy val possPlaceable = {
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
    * @param stone 石が置かれた座標 (0 to 63)
    */
  @inline def possToFlip(stone: Int) = {
    val omx = snd.code
    val omYZW = snd.code & 0x7e7e7e7e7e7e7e7eL
    var f = 0L;
    {
      val maskX = 0x0080808080808080L >>> 63 - stone
      val maskY = 0x7F00000000000000L >>> 63 - stone
      val maskZ = 0x0102040810204000L >>> 63 - stone
      val maskW = 0x0040201008040201L >>> 63 - stone
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
      val maskX = 0x0101010101010100L << stone
      val maskY = 0x00000000000000feL << stone
      val maskZ = 0x0002040810204080L << stone
      val maskW = 0x8040201008040200L << stone
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

  @inline def place(stone: Int): Board = {
    val flipped = possToFlip(stone)
    Board(snd & ~flipped.code, fst | flipped | (1L << stone), null, countEmpty - 1)
  }

  @inline override def equals(that: Any) = that match {
    case b: Board => fst == b.fst && snd == b.snd
    case _ => false
  }

  override def toString = toString(-1)

  def toString(newPos: Int) = {
    val fm = fst.toMatrix
    val sm = snd.toMatrix

    val bBlack = Ansi.bBlackStart
    // val bCyan = "\u001b[46m"
    val bDefault = "\u001b[0m"
    val bGreen = "\u001b[42m\u001b[37;48;5;22m"
    val bWhite = "\u001b[107m\u001b[30;48;5;231m"
    val bYellow = "\u001b[43m"

    s"Board($countFst-$countSnd-($countEmpty), ${fst.code}, ${snd.code}):\n" +
      s"$bGreen  1 2 3 4 5 6 7 8   $bBlack  \n" +
      fm.zip(sm).zipWithIndex.map { case ((fr, sr), i) =>
        bGreen + "ABCDEFGH" (i) + " " +
          (fr zip sr).zipWithIndex.map { case ((f, s), j) =>
            if(newPos == i * 8 + j) s"$bYellow  "
            else if(f) if(s) "!!" else s"$bWhite  " else if(s) s"$bBlack  " else s"$bGreen  "
          }.mkString +
          s"$bGreen " + "ABCDEFGH" (i) + s"$bBlack  "
      }.mkString("\n") +
      s"\n$bGreen  1 2 3 4 5 6 7 8   $bDefault"
  }

  def toLightBoard = new LightBoard(fst.code, snd.code)
}

object Board {

  @inline def apply(
    fst: Panel,
    snd: Panel,
    _pass: Board,
    countEmpty: Int
  ) = new Board(fst, snd, _pass, countEmpty)

  @inline def fromPanels(fst: Panel, snd: Panel) =
    new Board(
      fst, snd, null, BitUtil.popcount(~(fst.code | snd.code))
    )


  val InitialBoard = Board(
    Panel(0x0000000810000000L), Panel(0x0000001008000000L), null, 60
  )

  def fromMatrix(f: Seq[Seq[Boolean]], s: Seq[Seq[Boolean]]) = {
    fromPanels(Panel.fromMatrix(f), Panel.fromMatrix(s))
  }

  def fromScrLine(s: String) =
    fromPanels(
      Panel(s.reverseMap(c => if(c == 'O') 1L else 0L).reduce(_ << 1 | _)),
      Panel(s.reverseMap(c => if(c == 'X') 1L else 0L).reduce(_ << 1 | _))
    )

  sealed trait BoardResult {
    def unary_~ : BoardResult
  }

  final case class FstWin(diff: Int) extends BoardResult {
    def unary_~ = SndWin(diff)
  }

  final case class SndWin(diff: Int) extends BoardResult {
    def unary_~ = FstWin(diff)
  }

  case object Draw extends BoardResult {
    val unary_~ = Draw
  }

  case object NotEnd extends BoardResult {
    val unary_~ = NotEnd
  }

}
