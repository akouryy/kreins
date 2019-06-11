package net.akouryy.kreins
package game

import util.BitUtil

final case class Panel(code: Long) extends AnyVal {
  @inline def toMatrix: Seq[Seq[Boolean]] = {
    val r8 = 0 to 7
    r8.map { i =>
      r8.map { j =>
        (code >>> (i << 3 | j) & 1) == 1
      }
    }
  }

  @inline def &(p: Panel)(implicit d: DummyImplicit) = Panel(code & p.code)

  @inline def &(c: Long) = Panel(code & c)

  @inline def |(p: Panel)(implicit d: DummyImplicit) = Panel(code | p.code)

  @inline def |(c: Long) = Panel(code | c)

  @inline def moveRowUp = Panel(BitUtil.rotateRight(code, 8))

  @inline def popcount = BitUtil.popcount(code)

  /**
    * (https://primenumber.hatenadiary.jp/entry/2016/12/03/203823)
    * https://web.archive.org/web/20180823080354/https://chessprogramming.wikispaces.com/Flipping+Mirroring+and+Rotating
    */
  @inline def mirrorWithDiagRightDown = {
    val a = code
    val b = (a ^ a << 28) & 0x0f0f0f0f00000000L
    val c = a ^ b ^ b >>> 28
    val d = (c ^ c << 14) & 0x3333000033330000L
    val e = c ^ d ^ d >>> 14
    val f = (e ^ e << 7) & 0x5500550055005500L
    Panel(e ^ f ^ f >>> 7)
  }

  /**
    * (https://primenumber.hatenadiary.jp/entry/2016/12/03/203823)
    * https://web.archive.org/web/20180823080354/https://chessprogramming.wikispaces.com/Flipping+Mirroring+and+Rotating
    **/
  @inline def mirrorWithDiagRightUp = {
    val a = code
    val b = a ^ a << 36
    val c = a ^ (b ^ a >>> 36) & 0xf0f0f0f00f0f0f0fL
    val d = (c ^ c << 18) & 0xcccc0000cccc0000L
    val e = c ^ d ^ d >>> 18
    val f = (e ^ e << 9) & 0xaa00aa00aa00aa00L
    Panel(e ^ f ^ f >>> 9)
  }

  @inline def mirrorWithHorizontal =
    Panel(BitUtil.swapBytes(code))

  @inline def mirrorWithVertical =
    Panel(BitUtil.swapBitsInEachByte(code))

  /**
    * https://web.archive.org/web/20180823080354/https://chessprogramming.wikispaces.com/Flipping+Mirroring+and+Rotating
    */
  @inline def rotate45ACW = {
    val a = code
    val b = a ^ BitUtil.COLS_a1357 & (a ^ BitUtil.rotateRight(a, 8))
    val c = b ^ BitUtil.COLS_a2367 & (b ^ BitUtil.rotateRight(b, 16))
    val d = c ^ BitUtil.COLS_a4567 & (c ^ BitUtil.rotateRight(c, 32))
    Panel(d)
  }

  /**
    * https://web.archive.org/web/20180823080354/https://chessprogramming.wikispaces.com/Flipping+Mirroring+and+Rotating
    */
  @inline def rotate45CW = {
    val a = code
    val b = a ^ BitUtil.COLS_a0246 & (a ^ BitUtil.rotateRight(a, 8))
    val c = b ^ BitUtil.COLS_a0145 & (b ^ BitUtil.rotateRight(b, 16))
    val d = c ^ BitUtil.COLS_a0123 & (c ^ BitUtil.rotateRight(c, 32))
    Panel(d)
  }

  @inline def fixedRectangleCount = {
    var x = 0L
    val a = code
    locally {
      val b = a & (a >>> 1 | BitUtil.COLS_a7)
      val c = b & (b >>> 2 | BitUtil.COLS_a67)
      val d = c & (c >>> 4 | BitUtil.COLS_a4567)
      locally {
        val e = d & (d >>> 8 | BitUtil.ROWS_a7)
        val f = e & (e >>> 16 | BitUtil.ROWS_a67)
        val g = f & (f >>> 32 | BitUtil.ROWS_a4567)
        x |= g
      }
      locally {
        val e = d & (d << 8 | BitUtil.ROWS_a0)
        val f = e & (e << 16 | BitUtil.ROWS_a01)
        val g = f & (f << 32 | BitUtil.ROWS_a0123)
        x |= g
      }
    }
    locally {
      val b = a & (a << 1 | BitUtil.COLS_a0)
      val c = b & (b << 2 | BitUtil.COLS_a01)
      val d = c & (c << 4 | BitUtil.COLS_a0123)
      locally {
        val e = d & (d >>> 8 | BitUtil.ROWS_a7)
        val f = e & (e >>> 16 | BitUtil.ROWS_a67)
        val g = f & (f >>> 32 | BitUtil.ROWS_a4567)
        x |= g
      }
      locally {
        val e = d & (d << 8 | BitUtil.ROWS_a0)
        val f = e & (e << 16 | BitUtil.ROWS_a01)
        val g = f & (f << 32 | BitUtil.ROWS_a0123)
        x |= g
      }
    }
    BitUtil.popcount(x)
  }

  override def toString = {
    s"Panel($code):\n+---------------+\n" +
      toMatrix.map { r =>
        "|" +
          r.map { c =>
            if(c) '#' else ' '
          }.mkString(" ") +
          "|"
      }.mkString("\n") +
      "\n+---------------+"
  }
}

object Panel {
  @inline def fromMatrix(m: Seq[Seq[Boolean]]) = {
    // assert(m.length == 8 && m.forall(_.length == 8), "invalid matrix size")
    var c = 0L
    for(i <- 0 to 63) if(m(i >> 3)(i & 7)) c |= 1L << i
    Panel(c)
  }
}
