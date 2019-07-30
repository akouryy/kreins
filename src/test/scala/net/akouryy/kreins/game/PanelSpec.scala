package net.akouryy.kreins.game

import net.akouryy.kreins.util.BitUtil
import org.scalatest._

import scala.util.Random

class PanelSpec extends FlatSpec with DiagrammedAssertions {
  def genPanelMatrix(f: (Int, Int) => Boolean): Seq[Seq[Boolean]] =
    0.to(7).map(i => 0.to(7).map(j => f(i, j)))

  def testPanel(f: Panel => Panel, ansFn: (Seq[Seq[Boolean]], Int, Int) => Boolean) {
    for(t <- 0 until 100) {
      val xm =
        if(t == 0) {
          List.fill(8, 8)(false)
        } else if(t == 99) {
          List.fill(8, 8)(true)
        } else {
          List.fill(8, 8)(Random.nextInt(2) == 0)
        }
      val x = Panel.fromMatrix(xm)
      val ans = Panel.fromMatrix(genPanelMatrix(ansFn(xm, _, _)))
      assert(f(x) === ans)
    }
  }

  "fromMatrix" should "正しいパネルを返す" in {
    assert(Panel.fromMatrix(List.fill(8, 8)(false)) === Panel(0L))
    assert(Panel.fromMatrix(List.fill(8, 8)(true)) === Panel(-1L))
    assert(Panel.fromMatrix(genPanelMatrix(_ == 0 && _ == 0)) === Panel(1L))
    assert(Panel.fromMatrix(genPanelMatrix(_ == 7 && _ == 7)) === Panel(1L << 63))
    assert(Panel.fromMatrix(genPanelMatrix((i, _) => i <= 2)) === Panel((1L << 24) - 1))
  }

  "toMatrix" should "fromMatrixの逆関数である" in {
    for(_ <- 0 until 100) {
      val k = Panel(Random.nextLong())
      assert(Panel.fromMatrix(k.toMatrix) === k)
    }
    for(_ <- 0 until 100) {
      val k = genPanelMatrix((_, _) => Random.nextBoolean())
      assert(Panel.fromMatrix(k).toMatrix === k)
    }
  }

  "mirrorWithDiagRightDown" should "右上-左下反転した盤面を返す" in {
    testPanel(_.mirrorWithDiagRightDown, (m, i, j) => m(j)(i))
  }

  "mirrorWithDiagRightUp" should "左上-右下反転した盤面を返す" in {
    testPanel(_.mirrorWithDiagRightUp, (m, i, j) => m(7 - j)(7 - i))
  }

  "mirrorWithHorizontal" should "上下反転した盤面を返す" in {
    testPanel(_.mirrorWithHorizontal, (m, i, j) => m(7 - i)(j))
  }

  "mirrorWithVertical" should "左右反転した盤面を返す" in {
    testPanel(_.mirrorWithVertical, (m, i, j) => m(i)(7 - j))
  }

  "rotate45CW" should "時計回りに45度回転させて詰め込んだ盤面を返す" in {
    testPanel(_.rotate45CW, (m, i, j) => m(i - j - 1 & 7)(j))
  }

  "rotate45ACW" should "反時計回りに45度回転させて詰め込んだ盤面を返す" in {
    testPanel(_.rotate45ACW, (m, i, j) => m(i + j & 7)(j))
  }

  "fixedRectangleCount" should "確定石の数を数える" in {
    val er = Seq(0, 0, 0, 0, 0, 0, 0, 0)
    val pfs = Seq(
      Seq(er, er, er, er, er, er, er, er) -> 0,
      Seq(
        Seq(1, 1, 1, 0, 0, 0, 0, 0),
        Seq(1, 1, 0, 0, 0, 0, 0, 0),
        Seq(1, 0, 0, 0, 0, 0, 0, 0),
        er, er, er, er, er
      ) -> 6,
      Seq(
        Seq(1, 1, 1, 1, 1, 1, 1, 1),
        Seq(1, 1, 0, 0, 0, 0, 1, 1),
        Seq(1, 0, 0, 0, 0, 0, 0, 1),
        er, er, er, er, er
      ) -> 14,
      Seq(
        Seq(1, 0, 1, 0, 0, 0, 0, 0),
        Seq(1, 1, 0, 0, 0, 0, 0, 0),
        Seq(1, 0, 0, 0, 0, 0, 0, 0),
        er, er, er, er, er
      ) -> 3,
      Seq(
        Seq(1, 0, 1, 1, 1, 1, 1, 1),
        Seq(1, 1, 1, 0, 1, 1, 1, 1),
        Seq(1, 0, 0, 0, 0, 0, 0, 0),
        er, er, er, er, er
      ) -> 13
    )

    for((matI, frc) <- pfs) {
      val mat = matI.map(_.map(_ == 1))
      assert(Panel.fromMatrix(mat).fixedRectangleCount === frc)
    }
  }

  "aaa" should "bbb" in {
    var a = 0
    var i = 0
    val c0 = System.currentTimeMillis
    while(i < 10000000) {
      var pp = Random.nextLong
      while(pp != 0) {
        val j = BitUtil.firstHighBitPos(pp)
        a += j
        pp &= ~(1L << j)
      }
      i += 1
    }
    println(System.currentTimeMillis - c0)
    println(a)

    a = 0
    i = 0
    val c1 = System.currentTimeMillis
    while(i < 10000000) {
      var pp = Random.nextLong
      var j = 0
      while(pp != 0) {
        if(((pp >> j) & 1) == 1) {
          a += j
          pp &= ~(1L << j)
        }
        j += 1
      }
      i += 1
    }
    println(System.currentTimeMillis - c1)
    println(a)
  }
}
