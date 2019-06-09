/**
  * https://web.archive.org/web/20180823084812/http://chessprogramming.wikispaces.com/Proof-number+search
  * (https://blog.euphonictech.com/entry/2014/11/05/214050)
  * https://github.com/ysnrkdm/Graphene/blob/master/Sources/Graphene/SimpleProofSolver.swift
  */

package net.akouryy.kreins
package strategy

import model.{Board, Panel}
import util.BitUtil

import scala.collection.mutable

class CheckmateSearch(initialBoard: Board, isDrawOK: Boolean) {

  import CheckmateSearch._

  private[this] val MAX = 1000000000
  private[this] val MIN = -1000000000
  private[this] val evalMemo =
    mutable.HashMap[Board, Int]()

  private[this] val startTime = System.nanoTime

  var nNodes = 0

  def run = eval(initialBoard)

  def eval(b: Board) = {
    val root = Node(b, None, isOr = true)
    nNodes += 1
    checkCheckmate(root)
    updateNums(root)
    var curr = root
    while(root.nProof != 0 && root.nDisproof != 0 && resourcesAvailable()) {
      val bestProver = pickBestProver(curr)
      expandNode(bestProver)
      //println(bestProver.board, bestProver.children.length)
      curr = updateAncestors(bestProver, root)
    }
    if(root.state == Proven || root.nProof == 0) {
      MAX
    } else if(root.state == Disproven || root.nDisproof == 0) {
      MIN
    } else {
      0
    }
  }

  def checkCheckmate(n: Node) = {
    val b = n.board

    n.state =
      if(b.isEnd) {
        // n.isExpanded = false
        if(b.countBlack > b.countWhite) {
          if(n.isOr) Proven else Disproven
        } else if(b.countBlack < b.countWhite) {
          if(n.isOr) Disproven else Proven
        } else {
          if(isDrawOK) Proven else Disproven
        }
      } else {
        Unknown
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
        System.err.println(s"[CheckmateSearch#pickBestProver] " +
          s"Unreachable: empty bestNode\n$n")
        nNodes += 1
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

  def getChildren(n: Node): Array[Node] = {
    var pp = n.board.possPlaceable.code
    if(pp == 0) {
      // pass
      nNodes += 1
      return Array(Node(n.board.pass, Some(n), !n.isOr))
    }

    val children =
      mutable.ArrayBuilder.make[Node]
    while(pp != 0) {
      val i = BitUtil.firstHighBitPos(pp)
      children += Node(n.board.place(i, n.board.possToFlip(i)), Some(n), !n.isOr)
      nNodes += 1
      pp &= ~(1L << i)
    }
    children.result
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
      n.parent match {
        case Some(p) => n = p
        case None =>
          System.err.println(s"[CheckmateSearch#updateAncestors] Unreachable: no parent\n$n")
          return n
      }
    }
    updateNums(root)
    root
  }

  def resourcesAvailable() = System.nanoTime - startTime < 10000000000L /* 10000ms */

}

object CheckmateSearch {

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
