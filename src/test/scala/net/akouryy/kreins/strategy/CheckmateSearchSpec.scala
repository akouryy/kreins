package net.akouryy.kreins
package strategy

import org.scalatest._
import model.Board

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
          Some(Board.fromScrLine(s(0)), s(1).toInt)
        } else {
          throw new Exception("invalid score file")
        }
      }.toList.flatten
    )

  "eval" should "勝ちなら正の値、負けなら負の値、引き分けなら0を返す" in {
    var i = 0
    var maxNodes = 0
    var sumNodes = 0
    for((b, s) <- scores) {
      val ars = new CheckmateSearch(b, false)
      val t0 = System.nanoTime()
      val run = ars.run
      val t1 = System.nanoTime()
      assert(Integer.signum(run) === (if(s == 1) 1 else -1))
      i += 1
      sumNodes += ars.nNodes
      maxNodes = Math.max(maxNodes, ars.nNodes)
      println(f"$i%3s ${b.countEmpty}%2s ${ars.nNodes}%8s : ${
        java.text.NumberFormat.getIntegerInstance.format(t1 - t0)
      }%14sns")
    }
    println(s"max: $maxNodes, sum: $sumNodes")
  }
}
