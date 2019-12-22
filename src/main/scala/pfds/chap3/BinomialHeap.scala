package net.kinetc.pfds.chap3

import scala.math.Ordering
import scala.annotation.tailrec

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

  def rank: Int = this match { case BNNode(r, _, _) => r }
}

object BinomialTree {
  case class BNNode[Elem: Ordering](rankVal: Int, elem: Elem, child: List[BinomialTree[Elem]])
    extends BinomialTree[Elem]
}


sealed abstract class BinomialHeap[+Elem: Ordering] extends Heap[Elem, BinomialHeap, BinomialHeap[Elem]] {
  import BinomialHeap._
  import BinomialTree._

  override def isEmpty = this == BHNil

  override def insert[A >: Elem : Ordering](a: A): BinomialHeap[A] = {
    val node = BNNode(0, a, List())
    BinomialHeap.insTree(node, this)
  }

  override def merge[A >: Elem : Ordering](that: BinomialHeap[A]): BinomialHeap[A] = {
    (this, that) match {
      case (BHNil, _) => that
      case (_, BHNil) => this
      case (BHCons(h1, t1), BHCons(h2, t2)) =>
        if (h1.rank < h2.rank)
          BHCons(h1, t1.merge(that))
        else if (h2.rank < h1.rank)
          BHCons(h2, t2.merge(this))
        else
          insTree(h1.link(h2), t1.merge(t2))
    }
  }

  
}

object BinomialHeap extends HeapFactory[BinomialHeap] {
  def empty[A] = BHNil

  // To avoid Ordering bound for List[Nothing], make wrapper List explicitly
  case object BHNil extends BinomialHeap[Nothing]
  
  case class BHCons[Elem: Ordering](head: BinomialTree[Elem], tail: BinomialHeap[Elem]) extends BinomialHeap[Elem]

  @tailrec
  private def insTree[Elem : Ordering](node: BinomialTree[Elem], heap: BinomialHeap[Elem]): BinomialHeap[Elem] = {
    heap match {
      case BHNil => BHCons(node, BHNil)
      case BHCons(hd, tl) =>
        if (node.rank < hd.rank)
          BHCons(node, heap)
        else
          insTree(hd.link(node), tl)
    }
  }
}