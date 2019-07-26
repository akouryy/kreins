package net.akouryy.kreins.game

import org.scalatest._

import scala.util.Random

class BoardSpec extends FlatSpec with DiagrammedAssertions {
  def genPanelMatrix(f: (Int, Int) => Boolean): Seq[Seq[Boolean]] =
    0.to(7).map(i => 0.to(7).map(j => f(i, j)))

  def testBoardEnv[T](
    envFn: Board => T,
    fn: (Board, T) => Panel,
    ansFn: (Seq[Seq[Boolean]], Seq[Seq[Boolean]], Int, Int, T) => Boolean
  ) {
    for(t <- 0 until 100) {
      val xm_temp =
        if(t == 0) {
          List.fill(8, 8)(false)
        } else if(t == 99) {
          List.fill(8, 8)(true)
        } else {
          List.fill(8, 8)(Random.nextInt(if(t < 50) 2 else 3) == 0)
        }

      val ym =
        if(t == 0 || t == 99) {
          List.fill(8)(List.fill(8)(false))
        } else {
          List.fill(8, 8)(Random.nextInt(if(t < 50) 2 else 4) == 0)
        }

      val xm = xm_temp.zip(ym).map { case (xr, yr) =>
        xr.zip(yr).map { case (xc, yc) => xc && !yc }
      }

      val x = Panel.fromMatrix(xm)
      val y = Panel.fromMatrix(ym)
      val env = envFn(Board(x, y))
      val ans = Panel.fromMatrix(genPanelMatrix(ansFn(xm, ym, _, _, env)))
      assert(fn(Board(x, y), env) === ans)
    }
  }

  def testBoard(
    fn: Board => Panel,
    ansFn: (Seq[Seq[Boolean]], Seq[Seq[Boolean]], Int, Int) => Boolean
  ) = testBoardEnv[Unit](
    _ => (),
    (b, _) => fn(b),
    (fm, sm, i, j, _) => ansFn(fm, sm, i, j)
  )

  "possPuttableToLeft" should "石を置いて右側を返せる場所を返す" in {
    testBoard(_.possPlaceableToLeft, (am, bm, i, j) =>
      !am(i)(j) && !bm(i)(j) && (0 until j - 1).exists { k =>
        am(i)(k) && (k + 1 until j).forall(l => bm(i)(l))
      }
    )
  }

  "possPuttable" should "石を置ける場所を返す" in {
    testBoard(_.possPlaceable, (am, bm, i, j) =>
      !am(i)(j) && !bm(i)(j) &&
        (
          (j + 2 to 7).exists {
            k =>
              am(i)(k) && (j + 1 until k).forall(l => bm(i)(l))
          } ||
            (0 to j - 2).exists {
              k =>
                am(i)(k) && (k + 1 until j).forall(l => bm(i)(l))
            } ||
            (i + 2 to 7).exists {
              h =>
                am(h)(j) && (i + 1 until h).forall(g => bm(g)(j))
            } ||
            (0 to i - 2).exists {
              h =>
                am(h)(j) && (h + 1 until i).forall(g => bm(g)(j))
            } ||
            (2 to Math.min(7 - i, 7 - j)).exists {
              p =>
                am(i + p)(j + p) && (1 until p).forall(q => bm(i + q)(j + q))
            } ||
            (Math.max(-i, -j) to -2).exists {
              p =>
                am(i + p)(j + p) && (p + 1 to -1).forall(q => bm(i + q)(j + q))
            } ||
            (2 to Math.min(7 - i, j)).exists {
              p =>
                am(i + p)(j - p) && (1 until p).forall(q => bm(i + q)(j - q))
            } ||
            (Math.max(-i, j - 7) to -2).exists {
              p =>
                am(i + p)(j - p) && (p + 1 to -1).forall(q => bm(i + q)(j - q))
            }
          )
    )
  }

  "flipPoss" should "裏返る箇所を返す" in {
    var nLegal = 0

    while(nLegal < 100) {
      testBoardEnv[Int](
        b => {
          val p = b.possPlaceable
          if(p.code == 0L) {
            -1
          } else {
            nLegal += 1
            var r = Random.nextInt(p.popcount)
            (0 to 63).find { i =>
              if((p.code >>> i & 1) == 1L) {
                r -= 1
              }
              r == 0
            }.getOrElse(-1)
          }
        },
        _.possToFlip(_),
        (am, bm, i, j, pos) => {
          if(pos == -1) {
            /* skip case */
            (Board(Panel.fromMatrix(am), Panel.fromMatrix(bm)).possToFlip(-1)
              .code >>> (i << 3 | j) & 1) == 1
          } else {
            bm(i)(j) &&
              (-1 to 1).exists { di =>
                (-1 to 1).exists { dj =>
                  (di != 0 || dj != 0) && (1 to 7).exists { k =>
                    val il = i + di * k
                    val jl = j + dj * k
                    0 <= il && il <= 7 && 0 <= jl && jl <= 7 && pos == (il << 3 | jl) &&
                      (1 until k).forall { g => bm(i + di * g)(j + dj * g) }
                  } && (1 to 7).exists { k =>
                    val il = i - di * k
                    val jl = j - dj * k
                    0 <= il && il <= 7 && 0 <= jl && jl <= 7 && am(il)(jl) &&
                      (1 until k).forall { g => bm(i - di * g)(j - dj * g) }
                  }
                }
              }
          }
        }
      )
    }
  }
}
