package net.akouryy.kreins.encoder

import java.io.{InputStream, OutputStream}

object BytesEncoder {
  def encodeOne(o: OutputStream, bc: Int, bd: Long) {
    for(i <- bc - 1 to 0 by -1) {
      assert(0 <= bc && bc <= 8)
      o.write((bd >>> i * 8 & 255).toInt)
    }
  }

  def decodeOne(i: InputStream, bc: Int): Option[Long] = {
    assert(0 < bc && bc <= 8)
    var ans = 0L
    for(_ <- 0 until bc) {
      val r = i.read()
      if(r == -1) return None
      ans = ans << 8 | r
    }
    Some(ans)
  }
}
