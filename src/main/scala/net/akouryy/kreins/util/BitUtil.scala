package net.akouryy.kreins.util

//noinspection SpellCheckingInspection
@inline object BitUtil {
  val COLS_a0 = 0x0101010101010101L
  val COLS_a01 = 0x0303030303030303L
  val COLS_a0123 = 0x0f0f0f0f0f0f0f0fL
  val COLS_a0145 = 0x3333333333333333L
  val COLS_a0246 = 0x5555555555555555L
  val COLS_a1357 = 0xaaaaaaaaaaaaaaaaL
  val COLS_a2367 = 0xccccccccccccccccL
  val COLS_a4567 = 0xf0f0f0f0f0f0f0f0L
  val COLS_a67 = 0xc0c0c0c0c0c0c0c0L
  val COLS_a7 = 0x8080808080808080L
  val ROWS_a0 = 0x00000000000000ffL
  val ROWS_a01 = 0x000000000000ffffL
  val ROWS_a0123 = 0x00000000ffffffffL
  val ROWS_a4567 = 0xffffffff00000000L
  val ROWS_a67 = 0xffff000000000000L
  val ROWS_a7 = 0xff00000000000000L
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
    val b = a - (a >>> 1 & COLS_a0246)
    val c = (b >>> 2 & COLS_a0145) + (b & COLS_a0145)
    val d = (c >>> 4) + c & COLS_a0123
    (d * COLS_a0 >>> 56).toInt
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

  @inline def firstHighBitPos(a: Long) = {
    val b = a | a >>> 1
    val c = b | b >>> 2
    val d = c | c >>> 4
    val e = d | d >>> 8
    val f = e | e >>> 16
    val g = f | f >>> 32
    popcount(g) - 1
  }

  /**
    * https://qiita.com/kazatsuyu/items/38203287c19890a2b7c6
    */
  private[this] val LHBPMagic = 0x03f0a933adcbd8d1L
  private[this] val LHBPTable = Array(
    64, 0, -1, 1, -1, 12, -1, 2, 60, -1, 13, -1, -1, 53, -1, 3,
    61, -1, -1, 21, -1, 14, -1, 42, -1, 24, 54, -1, -1, 28, -1, 4,
    62, -1, 58, -1, 19, -1, 22, -1, -1, 17, 15, -1, -1, 33, -1, 43,
    -1, 50, -1, 25, 55, -1, -1, 35, -1, 38, 29, -1, -1, 45, -1, 5,
    63, -1, 11, -1, 59, -1, 52, -1, -1, 20, -1, 41, 23, -1, 27, -1,
    -1, 57, 18, -1, 16, -1, 32, -1, 49, -1, -1, 34, 37, -1, 44, -1,
    -1, 10, -1, 51, -1, 40, -1, 26, 56, -1, -1, 31, 48, -1, 36, -1,
    9, -1, 39, -1, -1, 30, 47, -1, 8, -1, -1, 46, 7, -1, 6
  )

  @inline def lastHighBitPos(a: Long) = {
    LHBPTable((LHBPMagic * (a & -a) >>> 57).toInt)
  }

  /**
    * (https://primenumber.hatenadiary.jp/entry/2016/12/03/203823)
    * https://web.archive.org/web/20180823080354/https://chessprogramming.wikispaces.com/Flipping+Mirroring+and+Rotating
    */
  @inline def swapBitsInEachByte(a: Long) = {
    val b = a >>> 1 & COLS_a0246 | (a & COLS_a0246) << 1
    val c = b >>> 2 & COLS_a0145 | (b & COLS_a0145) << 2
    c >>> 4 & COLS_a0123 | (c & COLS_a0123) << 4
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
