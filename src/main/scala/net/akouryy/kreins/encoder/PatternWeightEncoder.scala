package net.akouryy.kreins
package encoder

import java.io.{FileInputStream, FileOutputStream}

import strategy.Pattern

object PatternWeightEncoder {
  def encode(o: FileOutputStream, wss: Seq[Array[Byte]]) {
    wss.foreach(o.write)
  }

  def decode(i: FileInputStream) =
    Pattern.patterns.map { p =>
      val a = Array.ofDim[Byte](p.codeLimit)
      i.read(a)
      a
    }
}
