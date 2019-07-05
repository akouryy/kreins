package net.akouryy.kreins
package strategy

import game.{Board, Panel}
import util.BitUtil

import scala.collection.mutable

/**
  * https://web.archive.org/web/20180823084812/http://chessprogramming.wikispaces.com/Proof-number+search
  * (https://blog.euphonictech.com/entry/2014/11/05/214050)
  * https://github.com/ysnrkdm/Graphene/blob/master/Sources/Graphene/SimpleProofSolver.swift
  */
final class CheckmateSearch(isDrawOK: Boolean) {

  import CheckmateSearch._

  private[this] val MAX = 100000000
  private[this] val MIN = -100000000

  private[this] var startTimeMS = System.currentTimeMillis

  /*private[this] val nodeMemo =
    mutable.Map[Board, Node]()*/

  def run(initialBoard: Board, maxTimeMS: Long): RunResult = {
    startTimeMS = System.currentTimeMillis

    val root = /*nodeMemo.getOrElseUpdate(
      initialBoard,*/ Node(initialBoard, isOr = true)
    /*)*/
    root.parent = None

    checkCheckmate(root)
    updateNums(root)
    var curr = root
    while(root.nProof != 0 && root.nDisproof != 0 && resourcesAvailable(maxTimeMS)) {
      //      println("a")
      val bestProver = pickBestProver(curr)
      //      println("b")
      expandNode(bestProver)
      //      println("c")
      curr = updateAncestors(bestProver, root)
      //      println("d")
    }
    if(root.state == Proven || root.nProof == 0) {
      for(c <- root.children) if(c.state == Proven || c.nProof == 0) {
        var pp = root.board.possPlaceable.code
        if(pp == 0) {
          return WillWinPass
        }
        var x = 0
        while(pp != 0) {
          val i = BitUtil.firstHighBitPos(pp)
          if(c.board == root.board.place(i))
            return WillWin(i)
          pp &= ~(1L << i)
          x += 1
          if(x == 100) throw UnexpectedException(s"aaaaa $pp ${BitUtil.firstHighBitPos(pp)}")
        }
        throw UnexpectedException(s"[eval] no placement for children\n$c")
      }
      WillWinPass
    } else if(root.state == Disproven || root.nDisproof == 0) {
      WillLose
    } else {
      Timeout
    }
  }

  def checkCheckmate(n: Node) = {
    val b = n.board

    n.state =
      b.result1 match {
        case Board.FstWin(_) => if(n.isOr) Proven else Disproven
        case Board.SndWin(_) => if(n.isOr) Disproven else Proven
        case Board.Draw => if(isDrawOK) Proven else Disproven
        case Board.NotEnd => Unknown
      }
  }

  def updateNums(n: Node) {
    if(n.isExpanded) {
      if(n.isOr) {
        n.nProof = MAX
        n.nDisproof = 0
        for(c <- n.children) {
          n.nProof = n.nProof.min(c.nProof)
          n.nDisproof += c.nDisproof
        }
        n.nDisproof = n.nDisproof.min(MAX)
      } else {
        n.nProof = 0
        n.nDisproof = MAX
        for(c <- n.children) {
          n.nProof += c.nProof
          n.nDisproof = n.nDisproof.min(c.nDisproof)
        }
        n.nProof = n.nProof.min(MAX)
      }
    } else {
      n.state match {
        case Proven =>
          n.nProof = 0
          n.nDisproof = MAX
        case Disproven =>
          n.nProof = MAX
          n.nDisproof = 0
        case Unknown =>
          n.nProof = 1
          n.nDisproof = 1
      }
    }
  }

  def pickBestProver(_node: Node): Node = {
    var n = _node
    var i = 0
    while(n.isExpanded) {
      if(n.board.isEnd) {
        checkCheckmate(n)
        return n
      }

      var bestNum = MAX + 1
      var bestNode =
        n.children.headOption.getOrElse {
          Console.err.println(s"[pickBestProver] no children found: $n")
          return n
        }

      if(n.isOr) {
        for(c <- n.children) if(bestNum > c.nProof) {
          bestNum = c.nProof
          bestNode = c
        }
      } else {
        for(c <- n.children) if(bestNum > c.nDisproof) {
          bestNum = c.nDisproof
          bestNode = c
        }
      }

      bestNode.parent = Some(n)
      n = bestNode

      i += 1
      if(i >= 100) {
        Console.err.println(s"[pickBestProver] too many loop ($n")
        return n
      }
    }
    n
  }

  def expandNode(n: Node) {
    n.children = getChildren(n)
    for(c <- n.children) {
      c.parent = Some(n)
      checkCheckmate(c)
      updateNums(c)
      if(if(c.isOr) c.nProof == 0 else c.nDisproof == 0) {
        n.isExpanded = true
        return
      }
    }
    n.isExpanded = true
  }

  def getChildren(n: Node) = {
    var pp = n.board.possPlaceable.code
    if(pp == 0) {
      // pass
      val bp = n.board.pass
      val np = /*nodeMemo.getOrElseUpdate(
        bp, */ Node(bp, !n.isOr) /*
      )*/
      np.parent = Some(n)
      List(np)
    } else {
      var children = List[Node]()
      var x = 0
      while(pp != 0) {
        val i = BitUtil.firstHighBitPos(pp)
        val bp = n.board.place(i)
        val np = /*nodeMemo.getOrElseUpdate(
          bp,*/ Node(bp, !n.isOr)
        /*)*/
        np.parent = Some(n)
        children ::= np
        pp &= ~(1L << i)
        x += 1
        if(x == 100) throw UnexpectedException(s"aaaaa $pp ${BitUtil.firstHighBitPos(pp)}")
      }
      children
    }
  }

  def updateAncestors(_node: Node, root: Node): Node = {
    var n = _node
    var stopChecker = 0
    while(n != root) {
      val np = n.nProof
      val nd = n.nDisproof
      updateNums(n)
      if(n.nProof == np && n.nDisproof == nd) {
        return n
      }
      n = n.parent.getOrElse(throw UnexpectedException(
        s"[updateAncestors] no parent\n$n"
      ))
      stopChecker += 1
      if(stopChecker >= 1000) {
        println(n, n.parent)
        println(n.parent.get.parent)
        sys.exit(9)
        return root
      }
    }
    updateNums(root)
    root
  }

  def resourcesAvailable(maxTimeMS: Long) =
    System.currentTimeMillis - startTimeMS < maxTimeMS

  def nNodes = 0 //nodeMemo.size
}

object CheckmateSearch {

  case class UnexpectedException(msg: String = null, cause: Throwable = null)
    extends RuntimeException(msg, cause)

  sealed trait RunResult

  final case class WillWin(stone: Int) extends RunResult

  case object WillWinPass extends RunResult

  case object WillLose extends RunResult

  case object Timeout extends RunResult

  sealed trait ProofState {
    def unary_~ : ProofState
  }

  case object Proven extends ProofState {
    val unary_~ = Disproven
  }

  case object Disproven extends ProofState {
    val unary_~ = Proven
  }

  case object Unknown extends ProofState {
    val unary_~ = Unknown
  }

  case class Node(board: Board, isOr: Boolean) {
    var nProof = 1
    var nDisproof = 1
    var isExpanded = false
    var state: ProofState = Unknown
    var parent: Option[Node] = None
    var children = Seq[Node]()

    def toLongString =
      s"Node(nProof=$nProof, nDisproof=$nDisproof, " +
        s"isOr=$isOr, isExpanded=$isExpanded, " +
        s"state=$state, " +
        s"children:${children.length},\n" +
        s"$board,\n$parent)"

    override def toString =
      s"Node(nProof=$nProof, nDisproof=$nDisproof, " +
        s"isOr=$isOr, isExpanded=$isExpanded, " +
        s"state=$state, " +
        s"children:${children.length},\n" +
        s"$board)"
  }

}
