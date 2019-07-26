package net.akouryy.kreins
package strategy

import game.{Board, Panel}
import util.{BitUtil, ExtInt}

import scala.collection.mutable

/**
  * https://link.springer.com/content/pdf/10.1007%2F978-3-540-87608-3.pdf
  * https://ci.nii.ac.jp/naid/110002726401
  * https://www.chessprogramming.org/Proof-Number_Search
  * (https://blog.euphonictech.com/entry/2014/11/05/214050)
  * https://github.com/ysnrkdm/Graphene/blob/master/Sources/Graphene/SimpleProofSolver.swift
  */
final class CheckmateSearch(isDrawOK: Boolean) {

  import CheckmateSearch._

  private[this] val MAX = 100000000

  private[this] var startTimeMS = System.currentTimeMillis
  private[this] var maxTimeMS = 500L

  private[this] val memo = mutable.Map[Board, (Int, Int)]()

  @inline private[this] def retrieve(n: Node) = memo.getOrElse(n.board, (1, 1))

  @inline private[this] def store(n: Node, pr: Int, dpr: Int) {
    memo(n.board) = (pr, dpr)
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

  def run(initialBoard: Board, maxTimeMS: Long): RunResult = {
    startTimeMS = System.currentTimeMillis
    this.maxTimeMS = maxTimeMS

    memo.clear()

    val root = Node(initialBoard, isOr = true)
    root.thProof = MAX
    root.thDisproof = MAX
    dfWpn(root)
    val (pr, dpr) = retrieve(root)

    // println(pr, dpr)

    if(pr == 0) {
      for(c <- root.children) if(retrieve(c)._1 == 0) {
        var pp = root.board.possPlaceable.code
        if(pp == 0) {
          return WillWinPass
        }
        while(pp != 0) {
          val i = BitUtil.firstHighBitPos(pp)
          if(c.board == root.board.place(i))
            return WillWin(i)
          pp &= ~(1L << i)
        }
        Console.err.println(s"[CMSearch.run] no placement for children\n$c")
        return Timeout
      }
      WillWinPass
    } else if(dpr == 0) {
      WillLose
    } else {
      Timeout
    }
  }

  private[this] def dfWpn(n: Node) {
    var (pr, dpr) = retrieve(n)
    if(n.thProof <= pr || n.thDisproof <= dpr) {
      n.thProof = pr
      n.thDisproof = dpr
      return
    }
    n.board.result match {
      case Board.FstWin(_) =>
        storeProven(n)
      case Board.SndWin(_) =>
        storeDisproven(n)
      case Board.Draw =>
        if(isDrawOK == n.isOr /* XNOR */ ) storeProven(n) else storeDisproven(n)
      case Board.NotEnd =>
        generateMoves(n)
        store(n, n.thProof, n.thDisproof)
        while(true) {
          pr = dprMin(n)
          dpr = prMax(n)
          if(n.thProof <= pr || n.thDisproof <= dpr || !resourcesAvailable()) {
            n.thProof = pr
            n.thDisproof = dpr
            store(n, pr, dpr)
            return
          }
          val (child, prc, dpr2) = select(n)
          child.thProof = (n.thDisproof + prc - dpr).clamp(0, MAX)
          child.thDisproof = n.thProof.min(dpr2 + 1).clamp(0, MAX)
          dfWpn(child)
        }
    }
  }

  private[this] def select(n: Node): (Node, Int, Int) = {
    var prC, dprC, dpr2 = MAX
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

  @inline private[this] def prMax(n: Node) =
    (
      n.children.map(retrieve(_)._1).max +
        (n.children.count(c =>
          retrieve(c)._1 != 0 && retrieve(c)._2 != 0
        ) - 1).clampLow(0)
      ).clampHigh(MAX)

  @inline private[this] def dprMin(n: Node) =
    n.children.map(retrieve(_)._2).min

  private[this] def generateMoves(n: Node) {
    if(n.children.isEmpty) {
      var pp = n.board.possPlaceable.code
      if(pp == 0) {
        // pass
        n.children = List(Node(n.board.pass, !n.isOr))
      } else {
        n.children = List[Node]()
        while(pp != 0) {
          val i = BitUtil.firstHighBitPos(pp)
          n.children ::= Node(n.board.place(i), !n.isOr)
          pp &= ~(1L << i)
        }
      }
    } else {
      for(c <- n.children) {
        c.thProof = 1
        c.thDisproof = 1
      }
    }
  }

  @inline private[this] def resourcesAvailable() =
    System.currentTimeMillis - startTimeMS < maxTimeMS

  val nNodes = 0 //nodeMemo.size
}

object CheckmateSearch {

  case class UnexpectedException(msg: String = null, cause: Throwable = null)
    extends RuntimeException(msg, cause)

  sealed trait RunResult

  final case class WillWin(stone: Int) extends RunResult

  case object WillWinPass extends RunResult

  case object WillLose extends RunResult

  case object Timeout extends RunResult

  case class Node(board: Board, isOr: Boolean) {
    var thProof = 1
    var thDisproof = 1
    var children = List[Node]()

    override def toString =
      s"Node(${if(isOr) "or" else "and"}; $thProof / $thDisproof; ${children.size} " +
        s"children; $board)"
  }

}
