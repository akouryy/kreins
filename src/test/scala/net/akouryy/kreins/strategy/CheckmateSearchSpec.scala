package net.akouryy.kreins
package strategy

import org.scalatest._
import game.Board

class CheckmateSearchSpec extends FlatSpec with DiagrammedAssertions {
  val scores =
    util.Loan(scala.io.Source.fromFile(
      "src/test/scala/net/akouryy/kreins/strategy/endgame.scr1"
    ))(
      _.getLines().map { l =>
        val s = l.split(' ')
        if(s.isEmpty) {
          None
        } else if(s.length == 2) {
          Some((Board.fromScrLine(s(0)), s(1).toInt))
        } else {
          throw new Exception("invalid score file")
        }
      }.toList.flatten
    )

  "eval" should "勝ちなら正の値、負けなら負の値、引き分けなら0を返す" in {
    var i = 0
    var maxNodes = 0
    var sumNodes = 0
    var maxTime = 0L
    var sumTime = 0L
    for {
      (b, s) <- scores
      isDrawOK <- Seq(false, true)
      ars = new CheckmateSearch(isDrawOK)
    } {
      val t0 = System.nanoTime()
      val result = {
        import CheckmateSearch._
        ars.run(b, 10000)._1 match {
          case WillWin => true
          case WillLose => false
          case Timeout => throw new RuntimeException("timeout")
        }
      }
      val t1 = System.nanoTime()
      assert(result === (s == 1 || isDrawOK && s == 0))
      sumNodes += ars.nNodes
      maxNodes = Math.max(maxNodes, ars.nNodes)
      sumTime += t1 - t0
      maxTime = Math.max(maxTime, t1 - t0)
      println(f"${i / 2}%3s ${b.countEmpty}%2s ${ars.nNodes}%8sN ${ars.nLoops}%8sL : ${
        java.text.NumberFormat.getIntegerInstance.format(t1 - t0)
      }%14sns")
      i += 1
    }
    println(s"nodes max: $maxNodes; sum: $sumNodes\ntime max: ${
      java.text.NumberFormat.getIntegerInstance.format(maxTime)
    }ns; sum: ${
      java.text.NumberFormat.getIntegerInstance.format(sumTime)
    }ns")
  }
}
