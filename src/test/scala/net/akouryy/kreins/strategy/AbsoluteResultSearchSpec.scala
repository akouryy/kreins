package net.akouryy.kreins
package strategy

import org.scalatest._
import model.Board

class AbsoluteResultSearchSpec extends FlatSpec with DiagrammedAssertions {
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
    for((b, s) <- scores) {
      assert(Integer.signum(new AbsoluteResultSearch(b).run) === s)
      i += 1
      println(s"$i ${b.countEmpty}")
    }
  }
}
