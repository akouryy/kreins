package net.akouryy.kreins

package object util {

  implicit class ExtInt(val v: Int) extends AnyVal {
    @inline def clamp(low: Int, high: Int) =
      if(v < low) low else if(v > high) high else v

    @inline def clampLow(low: Int) = v max low

    @inline def clampHigh(high: Int) = v min high
  }

}
