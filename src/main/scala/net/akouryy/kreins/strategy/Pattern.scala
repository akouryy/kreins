package net.akouryy.kreins
package strategy

import game.Board

sealed trait Pattern {
  val id: Int
  val codeLimit: Int

  def code(b: Board): Int
}

object Pattern {
  val patterns = Array(
    Diag5Pattern,
    Diag6Pattern,
    Diag7Pattern,
    Diag8Pattern,
    Hor2Pattern,
    Hor3Pattern,
    Hor4Pattern,
    Edge2XPattern,
    Corner25Pattern,
    Corner33Pattern
  )
  assert(patterns.zipWithIndex.forall { case (p, i) => p.id == i })

  case object Diag5Pattern extends Pattern {
    val id = 0
    val codeLimit = math.pow(3, 5).toInt

    def code(b: Board) = {
      val f = b.fst.code
      val s = b.snd.code
      ((((f >>> 4 & 1 | s >>> 3 & 2) * 3 +
        (f >>> 11 & 1 | s >>> 10 & 2)) * 3 +
        (f >>> 18 & 1 | s >>> 17 & 2)) * 3 +
        (f >>> 25 & 1 | s >>> 24 & 2)) * 3 +
        (f >>> 32 & 1 | s >>> 31 & 2)
    }.toInt
  }

  case object Diag6Pattern extends Pattern {
    val id = 1
    val codeLimit = math.pow(3, 6).toInt

    def code(b: Board) = {
      val f = b.fst.code
      val s = b.snd.code
      (((((f >>> 5 & 1 | s >>> 4 & 2) * 3 +
        (f >>> 12 & 1 | s >>> 11 & 2)) * 3 +
        (f >>> 19 & 1 | s >>> 18 & 2)) * 3 +
        (f >>> 26 & 1 | s >>> 25 & 2)) * 3 +
        (f >>> 33 & 1 | s >>> 32 & 2)) * 3 +
        (f >>> 40 & 1 | s >>> 39 & 2)
    }.toInt
  }

  case object Diag7Pattern extends Pattern {
    val id = 2
    val codeLimit = math.pow(3, 7).toInt

    def code(b: Board) = {
      val f = b.fst.code
      val s = b.snd.code
      ((((((f >>> 6 & 1 | s >>> 5 & 2) * 3 +
        (f >>> 13 & 1 | s >>> 12 & 2)) * 3 +
        (f >>> 20 & 1 | s >>> 19 & 2)) * 3 +
        (f >>> 27 & 1 | s >>> 26 & 2)) * 3 +
        (f >>> 34 & 1 | s >>> 33 & 2)) * 3 +
        (f >>> 41 & 1 | s >>> 40 & 2)) * 3 +
        (f >>> 48 & 1 | s >>> 47 & 2)
    }.toInt
  }

  case object Diag8Pattern extends Pattern {
    val id = 3
    val codeLimit = math.pow(3, 8).toInt

    def code(b: Board) = {
      val f = b.fst.code
      val s = b.snd.code
      (((((((f >>> 7 & 1 | s >>> 6 & 2) * 3 +
        (f >>> 14 & 1 | s >>> 13 & 2)) * 3 +
        (f >>> 21 & 1 | s >>> 20 & 2)) * 3 +
        (f >>> 28 & 1 | s >>> 27 & 2)) * 3 +
        (f >>> 35 & 1 | s >>> 34 & 2)) * 3 +
        (f >>> 42 & 1 | s >>> 41 & 2)) * 3 +
        (f >>> 49 & 1 | s >>> 48 & 2)) * 3 +
        (f >>> 56 & 1 | s >>> 55 & 2)
    }.toInt
  }

  case object Hor2Pattern extends Pattern {
    val id = 4
    val codeLimit = math.pow(3, 8).toInt

    def code(b: Board) = {
      val f = b.fst.code
      val s = b.snd.code
      (((((((f >>> 8 & 1 | s >>> 7 & 2) * 3 +
        (f >>> 9 & 1 | s >>> 8 & 2)) * 3 +
        (f >>> 10 & 1 | s >>> 9 & 2)) * 3 +
        (f >>> 11 & 1 | s >>> 10 & 2)) * 3 +
        (f >>> 12 & 1 | s >>> 11 & 2)) * 3 +
        (f >>> 13 & 1 | s >>> 12 & 2)) * 3 +
        (f >>> 14 & 1 | s >>> 13 & 2)) * 3 +
        (f >>> 15 & 1 | s >>> 14 & 2)
    }.toInt
  }

  case object Hor3Pattern extends Pattern {
    val id = 5
    val codeLimit = math.pow(3, 8).toInt

    def code(b: Board) = {
      val f = b.fst.code
      val s = b.snd.code
      (((((((f >>> 16 & 1 | s >>> 15 & 2) * 3 +
        (f >>> 17 & 1 | s >>> 16 & 2)) * 3 +
        (f >>> 18 & 1 | s >>> 17 & 2)) * 3 +
        (f >>> 19 & 1 | s >>> 18 & 2)) * 3 +
        (f >>> 20 & 1 | s >>> 19 & 2)) * 3 +
        (f >>> 21 & 1 | s >>> 20 & 2)) * 3 +
        (f >>> 22 & 1 | s >>> 21 & 2)) * 3 +
        (f >>> 23 & 1 | s >>> 22 & 2)
    }.toInt
  }

  case object Hor4Pattern extends Pattern {
    val id = 6
    val codeLimit = math.pow(3, 8).toInt

    def code(b: Board) = {
      val f = b.fst.code
      val s = b.snd.code
      (((((((f >>> 24 & 1 | s >>> 23 & 2) * 3 +
        (f >>> 25 & 1 | s >>> 24 & 2)) * 3 +
        (f >>> 26 & 1 | s >>> 25 & 2)) * 3 +
        (f >>> 27 & 1 | s >>> 26 & 2)) * 3 +
        (f >>> 28 & 1 | s >>> 27 & 2)) * 3 +
        (f >>> 29 & 1 | s >>> 28 & 2)) * 3 +
        (f >>> 30 & 1 | s >>> 29 & 2)) * 3 +
        (f >>> 31 & 1 | s >>> 30 & 2)
    }.toInt
  }

  case object Edge2XPattern extends Pattern {
    val id = 7
    val codeLimit = math.pow(3, 10).toInt

    def code(b: Board) = {
      val f = b.fst.code
      val s = b.snd.code
      (((((((((f & 1 | s << 1 & 2) * 3 +
        (f >>> 1 & 1 | s & 2)) * 3 +
        (f >>> 2 & 1 | s >>> 1 & 2)) * 3 +
        (f >>> 3 & 1 | s >>> 2 & 2)) * 3 +
        (f >>> 4 & 1 | s >>> 3 & 2)) * 3 +
        (f >>> 5 & 1 | s >>> 4 & 2)) * 3 +
        (f >>> 6 & 1 | s >>> 5 & 2)) * 3 +
        (f >>> 7 & 1 | s >>> 6 & 2)) * 3 +
        (f >>> 9 & 1 | s >>> 8 & 2)) * 3 +
        (f >>> 14 & 1 | s >>> 13 & 2)
    }.toInt
  }

  case object Corner25Pattern extends Pattern {
    val id = 8
    val codeLimit = math.pow(3, 10).toInt

    def code(b: Board) = {
      val f = b.fst.code
      val s = b.snd.code
      (((((((((f & 1 | s << 1 & 2) * 3 +
        (f >>> 1 & 1 | s & 2)) * 3 +
        (f >>> 2 & 1 | s >>> 1 & 2)) * 3 +
        (f >>> 3 & 1 | s >>> 2 & 2)) * 3 +
        (f >>> 4 & 1 | s >>> 3 & 2)) * 3 +
        (f >>> 8 & 1 | s >>> 7 & 2)) * 3 +
        (f >>> 9 & 1 | s >>> 8 & 2)) * 3 +
        (f >>> 10 & 1 | s >>> 9 & 2)) * 3 +
        (f >>> 11 & 1 | s >>> 10 & 2)) * 3 +
        (f >>> 12 & 1 | s >>> 11 & 2)
    }.toInt
  }

  case object Corner33Pattern extends Pattern {
    val id = 9
    val codeLimit = math.pow(3, 9).toInt

    def code(b: Board) = {
      val f = b.fst.code
      val s = b.snd.code
      ((((((((f & 1 | s << 1 & 2) * 3 +
        (f >>> 1 & 1 | s & 2)) * 3 +
        (f >>> 2 & 1 | s >>> 1 & 2)) * 3 +
        (f >>> 8 & 1 | s >>> 7 & 2)) * 3 +
        (f >>> 9 & 1 | s >>> 8 & 2)) * 3 +
        (f >>> 10 & 1 | s >>> 9 & 2)) * 3 +
        (f >>> 16 & 1 | s >>> 15 & 2)) * 3 +
        (f >>> 17 & 1 | s >>> 16 & 2)) * 3 +
        (f >>> 18 & 1 | s >>> 17 & 2)
    }.toInt
  }

}
