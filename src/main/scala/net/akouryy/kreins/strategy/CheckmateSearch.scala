package net.akouryy.kreins
package strategy

import game.{Board, LightBoard}
import net.akouryy.kreins.util.ConsoleUtil.Ansi
import util.{BitUtil, ExtInt}

import scala.collection.mutable

/**
  * [1] https://link.springer.com/content/pdf/10.1007%2F978-3-540-87608-3.pdf
  * [2] https://ci.nii.ac.jp/naid/110002726401
  * https://www.chessprogramming.org/Proof-Number_Search
  * (https://blog.euphonictech.com/entry/2014/11/05/214050)
  * https://github.com/ysnrkdm/Graphene/blob/master/Sources/Graphene/SimpleProofSolver.swift
  */
final class CheckmateSearch(isDrawOK: Boolean) {

  import CheckmateSearch._

  private[this] val MAX = 100000000

  private[this] var endTimeMS = System.currentTimeMillis

  private[this] var memo: Array[mutable.Map[LightBoard, (Int, Int)]] = _

  @inline private[this] def retrieve(n: Node) = {
    if(n.board.countEmpty >= 2) {
      nLoops += 1
      memo(n.board.countEmpty).getOrElse(n.board.toLightBoard, (1, 1))
    } else {
      n.board.result1 match {
        case Board.FstWin(_) =>
          (0, MAX)
        case Board.SndWin(_) =>
          (MAX, 0)
        case Board.Draw =>
          if(isDrawOK == n.isOr /* XNOR */ ) (0, MAX) else (MAX, 0)
        case Board.NotEnd =>
          if(Kreins.isDebug) Console.err.println(Ansi.bRed("ERROR: NOT END"))
          (1, 1)
      }
    }
  }

  @inline private[this] def store(n: Node, pr: Int, dpr: Int) {
    n.thProof = pr // [2] l.34
    n.thDisproof = dpr
    if(n.board.countEmpty >= 2)
      memo(n.board.countEmpty)(n.board.toLightBoard) = (pr, dpr)
  }

  @inline private[this] def storeProven(n: Node) {
    store(n, 0, MAX)
  }

  @inline private[this] def storeDisproven(n: Node) {
    store(n, MAX, 0)
  }

  def run(initialBoard: Board, maxTimeMS: Long): (RunResult, Int) = {
    endTimeMS = System.currentTimeMillis + maxTimeMS

    memo = Array.fill(initialBoard.countEmpty + 1)(mutable.Map[LightBoard, (Int, Int)]())
    nLoops = 0
    countRACall = 0
    couldResourcesBeAvailable = true

    val root = Node(initialBoard, isOr = true)
    root.thProof = MAX
    root.thDisproof = MAX
    dfWpn(root)
    nNodes = memo.map(_.size).sum
    memo = null

    val result =
      if(root.thProof == 0) WillWin
      else if(root.thDisproof == 0) WillLose
      else Timeout

    generateMoves(root) // for when root.isEnd1 but !root.isEnd

    if(root.children.isEmpty) return (result, -1)

    val bestBoard =
      root.children.find(_.thDisproof == 0)
        .getOrElse(root.children.maxBy(_.thProof))
        .board

    var bestPos = -1

    var pp = root.board.possPlaceable.code
    while(pp != 0) {
      val i = BitUtil.firstHighBitPos(pp)
      if(bestBoard == root.board.place(i)) {
        bestPos = i
      }
      pp &= ~(1L << i)
    }

    (result, bestPos)
  }

  private[this] def dfWpn(n: Node) {
    n.board.result1 match {
      case Board.FstWin(_) =>
        storeProven(n)
      case Board.SndWin(_) =>
        storeDisproven(n)
      case Board.Draw =>
        if(isDrawOK == n.isOr /* XNOR */ ) storeProven(n) else storeDisproven(n)
      case Board.NotEnd =>
        generateMoves(n)
        // store(n, n.thProof, n.thDisproof) // unnecessary because there is no cycle
        var prC0 = 0

        val retrieveMemo =
          n.children.map(c => c -> retrieve(c)).toArray
        while(true) {
          /* ***** prMax, dprMin; [1] ll.28-29, 67-85 ***** */
          var prCMax = 0
          var pr = MAX
          var cnt = -1
          locally {
            var i = 0
            val l = retrieveMemo.length
            while(i < l) {
              val (prC, dprC) = retrieveMemo(i)._2
              prCMax = prCMax max prC
              pr = pr min dprC
              if(prC != 0) cnt += 1
              i += 1
            }
          }
          val dpr = (prCMax + cnt.clampLow(0)).clampHigh(MAX)

          if(n.thProof <= pr || n.thDisproof <= dpr || !resourcesAvailable()) {
            // n.thProof = pr // [2] // moved to store()
            // n.thDisproof = dpr // [2] // moved to store()
            store(n, pr, dpr)
            return
          }
          val (ci, prC, dprC, dpr2) = select(prC0, retrieveMemo)
          prC0 = prC
          val child = retrieveMemo(ci)._1
          val prB = (n.thDisproof + prC - dpr).clamp(0, MAX)
          val dprB = (n.thProof min (dpr2 + 1)).clampHigh(MAX)
          if(prB <= prC || dprB <= dprC) { // [1]: ll 9-11
            child.thProof = prC
            child.thDisproof = dprC
          } else {
            child.thProof = prB
            child.thDisproof = dprB
            dfWpn(child)
            retrieveMemo(ci) = (child, (child.thProof, child.thDisproof))
          }
        }
    }
  }

  @inline private[this] def select(
    prC0: Int, retrieveMemo: Seq[(Node, (Int, Int))]
  ): (Int, Int, Int, Int) = {
    var prC = prC0
    var dprC, dpr2 = MAX
    var bestI = 0

    var i = 0
    while(i < retrieveMemo.length) {
      val (pr, dpr) = retrieveMemo(i)._2
      if(dpr < dprC) {
        bestI = i
        prC = pr
        dpr2 = dprC
        dprC = dpr
      } else if(dpr < dpr2) {
        dpr2 = dpr
      }
      if(pr == MAX) return (bestI, prC, dprC, dpr2)
      i += 1
    }
    (bestI, prC, dprC, dpr2)
  }

  @inline private[this] def generateMoves(n: Node) {
    if(n.children.isEmpty) {
      var pp = n.board.possPlaceable.code
      if(pp == 0) {
        // pass
        n.children = List(Node(n.board.pass, !n.isOr))
      } else {
        var c = List[Node]()
        while(pp != 0) {
          val i = BitUtil.firstHighBitPos(pp)
          c ::= Node(n.board.place(i), !n.isOr)
          pp &= ~(1L << i)
        }
        n.children = c
      }
    }
  }

  private[this] var couldResourcesBeAvailable = true

  private[this] var countRACall = 0

  @inline private[this] def resourcesAvailable() = {
    countRACall += 1

    couldResourcesBeAvailable =
      couldResourcesBeAvailable &&
        ((countRACall & 1023) != 1023 || System.currentTimeMillis < endTimeMS)
    couldResourcesBeAvailable
  }

  var nNodes = 0

  var nLoops = 0
}

object CheckmateSearch {

  sealed trait RunResult

  case object WillWin extends RunResult

  case object WillLose extends RunResult

  case object Timeout extends RunResult

  final case class Node(board: Board, isOr: Boolean) {
    var thProof = 1
    var thDisproof = 1
    var children = List[Node]()

    override def toString =
      s"Node(${if(isOr) "or" else "and"}; $thProof / $thDisproof; ${children.size} " +
        s"children; $board)"
  }

}
