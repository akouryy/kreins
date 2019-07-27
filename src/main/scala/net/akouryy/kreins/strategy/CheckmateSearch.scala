package net.akouryy.kreins
package strategy

import game.{Board, LightBoard}
import util.{BitUtil, ExtInt}

import scala.collection.mutable

/**
  * https://link.springer.com/content/pdf/10.1007%2F978-3-540-87608-3.pdf
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

  @inline private[this] def retrieve(n: Node) =
    memo(n.board.countEmpty).getOrElse(n.board.toLightBoard, (1, 1))

  @inline private[this] def store(n: Node, pr: Int, dpr: Int) {
    memo(n.board.countEmpty)(n.board.toLightBoard) = (pr, dpr)
  }

  @inline private[this] def storeProven(n: Node) = {
    n.thProof = 0
    n.thDisproof = MAX
    store(n, 0, MAX)
  }

  @inline private[this] def storeDisproven(n: Node) = {
    n.thProof = MAX
    n.thDisproof = 0
    store(n, MAX, 0)
  }

  def run(initialBoard: Board, maxTimeMS: Long): (RunResult, Int) = {
    endTimeMS = System.currentTimeMillis + maxTimeMS

    memo = Array.fill(initialBoard.countEmpty + 1)(mutable.Map[LightBoard, (Int, Int)]())
    nLoops = 0

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
    nLoops += 1
    var (pr, dpr) = retrieve(n)
    if(n.thProof <= pr || n.thDisproof <= dpr) {
      n.thProof = pr
      n.thDisproof = dpr
      return
    }
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
        while(true) {
          locally { // prMax, dprMin
            var prCMax = 0
            pr = MAX
            var cnt = -1
            for(c <- n.children) {
              val (prC, dprC) = retrieve(c)
              prCMax = prCMax max prC
              pr = pr min dprC
              if(prC != 0) cnt += 1
            }
            dpr = (prCMax + cnt.clampLow(0)).clampHigh(MAX)
          }
          if(n.thProof <= pr || n.thDisproof <= dpr || !resourcesAvailable()) {
            n.thProof = pr // [2]
            n.thDisproof = dpr // [2]
            store(n, pr, dpr)
            return
          }
          val (child, prC, dpr2) = select(n, prC0)
          prC0 = prC
          child.thProof = (n.thDisproof + prC - dpr).clamp(0, MAX)
          child.thDisproof = (n.thProof min (dpr2 + 1)).clampHigh(MAX)
          dfWpn(child)
        }
    }
  }

  @inline private[this] def select(n: Node, prC0: Int): (Node, Int, Int) = {
    var prC = prC0
    var dprC, dpr2 = MAX
    var best: Node = null

    for(c <- n.children) {
      val (pr, dpr) = retrieve(c)
      if(dpr < dprC) {
        best = c
        prC = pr
        dpr2 = dprC
        dprC = dpr
      } else if(dpr < dpr2) {
        dpr2 = dpr
      }
      if(pr == MAX) return (best, prC, dpr2)
    }
    (best, prC, dpr2)
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

  @inline private[this] def resourcesAvailable() =
    System.currentTimeMillis < endTimeMS

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
