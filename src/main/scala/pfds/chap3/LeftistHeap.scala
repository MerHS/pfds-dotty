package net.kinetc.pfds.chap3

import scala.math.Ordering

sealed abstract class LeftistHeap[+Elem: Ordering] extends Heap[Elem, LeftistHeap, LeftistHeap[Elem]] {
  import LeftistHeap._

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

  // ex 3.2
  def insertInline[A >: Elem : Ordering](a: A): LeftistHeap[A] = 
    this match {
      case LHEmpty => LHNode(1, a, LHEmpty, LHEmpty)
      case LHNode(_, x, l, r) =>
        if (summon[Ordering[A]].lt(x, a))
          makeT(x, l, r.insertInline(a))
        else
          makeT(a, l, r.insertInline(x))
    }
}

object LeftistHeap extends HeapFactory[LeftistHeap] {
  case class HeapEmptyException() extends Exception()

  case object LHEmpty extends LeftistHeap[Nothing]
  
  case class LHNode[Elem: Ordering](
    rankVal: Int, elem: Elem, lc: LeftistHeap[Elem], rc: LeftistHeap[Elem]
  ) extends LeftistHeap[Elem]

  def empty[A] = LHEmpty

  def makeT[Elem: Ordering](
    elem: Elem, h1: LeftistHeap[Elem], h2: LeftistHeap[Elem]
  ): LeftistHeap[Elem] = {
    if (h1.rank >= h2.rank) 
      LHNode(h2.rank + 1, elem, h1, h2)
    else 
      LHNode(h1.rank + 1, elem, h2, h1)
  }
}
