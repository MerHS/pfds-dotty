package net.kinetc.pfds.chap3

import scala.math.Ordering
import scala.annotation.tailrec


trait HeapFactory[T[A] <: Heap[A, T, T[A]]] {
  def empty[A]: T[A]

  @tailrec
  final def formHelper[A: Ordering](l: Iterator[T[A]]): Iterator[T[A]] = {
    if (!l.hasNext)  {
      return Iterator.empty
    }
    val fst = l.next
    if (!l.hasNext)  {
      return Iterator(fst)
    }
    val snd = l.next
    formHelper(Iterator(fst.merge(snd)) ++ l.sliding(2, 2).map({
      _.toList match {
        case t1 :: t2 :: Nil => 
          t1.merge(t2)
        case t :: Nil => t
        case _ => empty
      }
    }))
  }
  
  // ex 3.3
  def formList[A: Ordering](l: IterableOnce[A]): T[A] = {
    var iter = formHelper(l.iterator.map(empty.insert(_)))
    if (iter.hasNext) {
      iter.toList.head
    } else {
      empty
    }
  }
}

trait Heap[+Elem, CC[_], +C] {
  def isEmpty: Boolean

  def merge[A >: Elem : Ordering](that: CC[A]): CC[A]
  def insert[A >: Elem : Ordering](a: A): CC[A]
  
  def findMin: Elem
  def deleteMin: C
}