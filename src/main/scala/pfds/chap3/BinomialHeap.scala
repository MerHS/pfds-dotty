package net.kinetc.pfds.chap3

import scala.math.Ordering

sealed abstract class BinomialTree[+Elem: Ordering] {
  import BinomialTree._

  def link[B >: Elem : Ordering](that: BinomialTree[B]): BinomialTree[B] = {
    (this, that) match {
      case (BNNode(r1, x1, c1), BNNode(r2, x2, c2)) =>
        // assert(r1 == r2)
        if (summon[Ordering[B]].lteq(x1, x2))
          BNNode(r1 + 1, x2, that :: c1)
        else
          BNNode(r1 + 1, x2, this :: c2)
    }
  }
}

object BinomialTree {
  case class BNNode[Elem: Ordering](rank: Int, elem: Elem, child: List[BinomialTree[Elem]])
    extends BinomialTree[Elem]
}


sealed abstract class BinomialHeap[+Elem] extends Heap[Elem, BinomialHeap, BinomialHeap[Elem]] {
  import BinomialHeap._

  def isEmpty = this == BHNil
}

object BinomialHeap extends HeapFactory[BinomialHeap] {
  def empty[A] = BHNil

  // To avoid Ordering bound for List[Nothing], make wrapper List explicitly
  case object BHNil extends BinomialHeap[Nothing]
  case class BHCons[Elem: Ordering](head: Elem, tail: BinomialTree[Elem]) extends BinomialHeap[Elem]
}