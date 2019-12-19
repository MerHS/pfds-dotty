package net.kinetc.pfds
import scala.math.Ordering
import scala.annotation.tailrec

import net.kinetc.pfds.Util.given

object Chap3 {
  trait Heap[+Elem: Ordering, T[+_]] {
    def empty: T[Nothing]
    def isEmpty: Boolean

    def merge[A >: Elem : Ordering](that: T[A]): T[A]
    def insert[A >: Elem : Ordering](a: A): T[A]
    
    def findMin: Elem
    def deleteMin: T[Elem]
  }

  sealed abstract class LeftistHeap[+Elem: Ordering] extends Heap[Elem, LeftistHeap] {
    import LeftistHeap._

    override def empty = LHEmpty
    override def isEmpty = this == LHEmpty

    override def merge[A >: Elem : Ordering](that: LeftistHeap[A]): LeftistHeap[A] = {
      (this, that) match {
        case (LHEmpty, _) => that
        case (_, LHEmpty) => this
        case (LHNode(_, x1, l1, r1), LHNode(_, x2, l2, r2)) =>
          if (summon[Ordering[A]].lt(x1, x2))
            makeT(x1, l1, r1.merge(that))
          else
            makeT(x2, l2, this.merge(r2))
      }
    }

    override def insert[A >: Elem : Ordering](a: A): LeftistHeap[A] = 
      this.merge(LHNode(1, a, LHEmpty, LHEmpty))

    @throws[HeapEmptyException]
    override def findMin = this match {
      case LHEmpty => throw HeapEmptyException()
      case LHNode(_, x, _, _) => x
    }

    @throws[HeapEmptyException]
    override def deleteMin = this match {
      case LHEmpty => throw HeapEmptyException()
      case LHNode(_, _, l, r) => l.merge(r)
    }

    def rank: Int = this match {
      case LHEmpty => 0
      case LHNode(r, _, _, _) => r
    }
  }

  object LeftistHeap {
    case class HeapEmptyException() extends Exception()

    case object LHEmpty extends LeftistHeap[Nothing]
    
    case class LHNode[Elem: Ordering](
      rankVal: Int, elem: Elem, lc: LeftistHeap[Elem], rc: LeftistHeap[Elem]
    ) extends LeftistHeap[Elem]

    def makeT[Elem: Ordering](
      elem: Elem, h1: LeftistHeap[Elem], h2: LeftistHeap[Elem]
    ): LeftistHeap[Elem] = {
      if (h1.rank >= h2.rank) 
        LHNode(h2.rank + 1, elem, h1, h2)
      else 
        LHNode(h1.rank + 1, elem, h2, h1)
    }
  }


}