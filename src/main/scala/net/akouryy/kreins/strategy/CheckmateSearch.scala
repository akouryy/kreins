/**
  * https://web.archive.org/web/20180823084812/http://chessprogramming.wikispaces.com/Proof-number+search
  * (https://blog.euphonictech.com/entry/2014/11/05/214050)
  * https://github.com/ysnrkdm/Graphene/blob/master/Sources/Graphene/SimpleProofSolver.swift
  */

package net.akouryy.kreins
package strategy

import game.{Board, Panel}
import util.BitUtil

final class CheckmateSearch(initialBoard: Board, isDrawOK: Boolean, maxTimeMS: Int) {

  import CheckmateSearch._

  private[this] val MAX = 100000000
  private[this] val MIN = -100000000

  private[this] val startTimeMS = System.currentTimeMillis

  var nNodes = 0

  def run: RunResult = {
    val root = Node(initialBoard, None, isOr = true)
    nNodes += 1
    checkCheckmate(root)
    updateNums(root)
    var curr = root
    while(root.nProof != 0 && root.nDisproof != 0 && resourcesAvailable()) {
      val bestProver = pickBestProver(curr)
      expandNode(bestProver)
      curr = updateAncestors(bestProver, root)
    }
    if(root.state == Proven || root.nProof == 0) {
      for(c <- root.children) if(c.state == Proven || c.nProof == 0) {
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
          n.nProof = Math.min(n.nProof, c.nProof)
          n.nDisproof += c.nDisproof
        }
        n.nDisproof = Math.min(n.nDisproof, MAX)
      } else {
        n.nProof = 0
        n.nDisproof = MAX
        for(c <- n.children) {
          n.nProof += c.nProof
          n.nDisproof = Math.min(n.nDisproof, c.nDisproof)
        }
        n.nProof = Math.min(n.nProof, MAX)
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

  def pickBestProver(_node: Node) = {
    var n = _node
    while(n.isExpanded) {
      var bestNum = MAX + 1
      var bestNode =
        n.children.headOption.getOrElse(
          Node(Board(Panel(0), Panel(0)), None, isOr = false)
        )

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
      if(bestNode.board.countEmpty == 64) {
        nNodes += 1
        throw UnexpectedException(s"[pickBestProver] empty bestNode\n$n")
      }
      n = bestNode
    }
    n
  }

  def expandNode(n: Node) {
    n.children = getChildren(n)
    for(c <- n.children) {
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
      nNodes += 1
      List(Node(n.board.pass, Some(n), !n.isOr))
    } else {
      var children = List[Node]()
      while(pp != 0) {
        val i = BitUtil.firstHighBitPos(pp)
        children ::= Node(n.board.place(i), Some(n), !n.isOr)
        nNodes += 1
        pp &= ~(1L << i)
      }
      children
    }
  }

  def updateAncestors(_node: Node, root: Node): Node = {
    var n = _node
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
    }
    updateNums(root)
    root
  }

  def resourcesAvailable() = System.currentTimeMillis - startTimeMS < maxTimeMS
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

  case class Node(board: Board, parent: Option[Node], isOr: Boolean) {
    var nProof = 1
    var nDisproof = 1
    var isExpanded = false
    var state: ProofState = Unknown
    var children = Seq[Node]()

    override def toString =
      s"Node(nProof=$nProof, nDisproof=$nDisproof, " +
        s"isOr=$isOr, isExpanded=$isExpanded, " +
        s"state=$state, " +
        s"children:${children.length},\n" +
        s"$board,\n$parent)"
  }

}
