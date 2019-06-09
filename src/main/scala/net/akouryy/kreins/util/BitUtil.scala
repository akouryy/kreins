package net.akouryy.kreins.util

//noinspection SpellCheckingInspection
@inline object BitUtil {
  val COLS_0123 = 0xf0f0f0f0f0f0f0f0L
  val COLS_0145 = 0xccccccccccccccccL
  val COLS_0246 = 0xaaaaaaaaaaaaaaaaL
  val COLS_1357 = 0x5555555555555555L
  val COLS_2367 = 0x3333333333333333L
  val COLS_4567 = 0x0f0f0f0f0f0f0f0fL
  val COLS_7 = 0x0101010101010101L
  val ROT45ACW_MASK = 0xfefcf8f0e0c08000L
  val ROT45CW_MASK = 0x80c0e0f0f8fcfeffL
  val ROWS_0246 = 0xff00ff00ff00ff00L
  val ROWS_1357 = 0x00ff00ff00ff00ffL
  val ROWS_2367 = 0x0000ffff0000ffffL

  @inline def rotateRight(a: Long, b: Int) = a >>> b | a << 64 - b

  /**
    * https://en.wikipedia.org/wiki/Hamming_weight popcount64c
    */
  @inline def popcount(a: Long) = {
    val b = a - (a >>> 1 & COLS_1357)
    val c = (b >>> 2 & COLS_2367) + (b & COLS_2367)
    val d = (c >>> 4) + c & COLS_4567
    (d * COLS_7 >>> 56).toInt
  }

  /**
    * https://primenumber.hatenadiary.jp/entry/2016/12/26/063226
    */
  @inline def firstHighBit(a: Long) = {
    val b = a | a >>> 1
    val c = b | b >>> 2
    val d = c | c >>> 4
    val e = swapBytes(d & ~(d >>> 1))
    swapBytes(e & -e)
  }

  /**
    * (https://primenumber.hatenadiary.jp/entry/2016/12/03/203823)
    * https://web.archive.org/web/20180823080354/https://chessprogramming.wikispaces.com/Flipping+Mirroring+and+Rotating
    */
  @inline def swapBitsInEachByte(a: Long) = {
    val b = a >>> 1 & COLS_1357 | (a & COLS_1357) << 1
    val c = b >>> 2 & COLS_2367 | (b & COLS_2367) << 2
    c >>> 4 & COLS_4567 | (c & COLS_4567) << 4
  }

  /**
    * (https://primenumber.hatenadiary.jp/entry/2016/12/03/203823)
    * https://web.archive.org/web/20180823080354/https://chessprogramming.wikispaces.com/Flipping+Mirroring+and+Rotating
    */
  @inline def swapBytes(a: Long) = {
    val b = a >>> 8 & ROWS_1357 | (a & ROWS_1357) << 8
    val c = b >>> 16 & ROWS_2367 | (b & ROWS_2367) << 16
    c >>> 32 | c << 32
  }
}
