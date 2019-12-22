package net.kinetc.pfds.chap3

import scala.math.Ordering

// ex 3.4
sealed abstract class WBLHeap[+Elem: Ordering] extends Heap[Elem, WBLHeap, WBLHeap[Elem]] {
  import WBLHeap._

  override def isEmpty = this == WHEmpty

  override def merge[A >: Elem : Ordering](that: WBLHeap[A]): WBLHeap[A] = {
    (this, that) match {
      case (WHEmpty, _) => that
      case (_, WHEmpty) => this
      case (WHNode(c1, x1, l1, r1), WHNode(c2, x2, l2, r2)) =>
        if (summon[Ordering[A]].lt(x1, x2)) {
          if (l1.weight < r1.weight + that.weight)
            WHNode(c1 + c2, x1, r1.merge(that), l1)
          else
            WHNode(c1 + c2, x1, l1, r1.merge(that))
        } else {
          if (l2.weight < r2.weight + this.weight)
            WHNode(c1 + c2, x2, r2.merge(this), l2)
          else
            WHNode(c1 + c2, x2, l2, r2.merge(this))
        }
    }
  }

  override def insert[A >: Elem : Ordering](a: A): WBLHeap[A] = 
    this.merge(WHNode(1, a, WHEmpty, WHEmpty))

  @throws[HeapEmptyException]
  override def findMin = this match {
    case WHEmpty => throw HeapEmptyException()
    case WHNode(_, x, _, _) => x
  }

  @throws[HeapEmptyException]
  override def deleteMin = this match {
    case WHEmpty => throw HeapEmptyException()
    case WHNode(_, _, l, r) => l.merge(r)
  }

  def weight: Int = this match {
    case WHEmpty => 0
    case WHNode(c, _, _, _) => c
  }

}

object WBLHeap extends HeapFactory[WBLHeap] {
  case class HeapEmptyException() extends Exception()

  case object WHEmpty extends WBLHeap[Nothing]
  
  case class WHNode[Elem: Ordering](
    count: Int, elem: Elem, lc: WBLHeap[Elem], rc: WBLHeap[Elem]
  ) extends WBLHeap[Elem]

  def empty[A] = WHEmpty
}
