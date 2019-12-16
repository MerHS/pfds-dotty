package pfds

import scala.collection.immutable._
import math.Ordered

object Chap2 {
  // Fig 2.1 Stack type class
  trait SStack[T[_]] {
    case class Empty() extends Exception()
    case class SubScriptError(i: Int) extends Exception()

    def empty[A]: T[A]
    def [A](xs: T[A]) isEmpty: Boolean

    def [A, B >: A](elem: B) cons (s: T[A]): T[B]
    def [A](s: T[A]) head: A
    def [A](s: T[A]) tail: T[A]

    def [A, B >: A](elem: B) :: (s: T[A]): T[B] = elem.cons(s)

    def [A, B >: A](xs: T[A]) concat (ys: T[B]): T[B] = 
      if xs.isEmpty then ys else xs.head :: (xs.tail.concat(ys))

    def [A, B >: A](xs: T[A]) ++ (ys: T[B]): T[B] = xs.concat(ys)

    def [A, B >: A](s: T[A]) update (i: Int, y: B): T[B] =       
      if (s.isEmpty)
        throw SubScriptError(i)
      else if (i == 0)
        y :: s.tail
      else
        s.head :: s.update(i-1, y)
  }

  // I do not implement it not to contaminate original list implementation
  // given SStack[List] with
  //   val empty = List()
  //   def [A](xs: List[A]) isEmpty: Boolean = xs.isEmpty

  enum ConsList[+A] {
    case NIL
    case CONS(a: A, s: ConsList[A])
  }

  given SStack[ConsList] {
    import ConsList._

    override def empty[A] = NIL
    override def [A](xs: ConsList[A]) isEmpty = xs match {
      case NIL => true
      case _ => false
    }

    override def [A, B >: A](elem: B) cons (s: ConsList[A]) = CONS(elem, s)
    override def [A](s: ConsList[A]) head = s match {
      case NIL => throw Empty()
      case CONS(a, s) => a
    }
    override def [A](s: ConsList[A]) tail = s match {
      case NIL => throw Empty()
      case CONS(a, s) => s
    }
  }

  // Fig 2.7 Set type class
  trait SSet[T[_]] {
    val empty: T[Nothing]
    def [A, B >: A](x: B) insert (s: T[A]): T[B]
    def [A, B >: A](x: B) member (s: T[A]): Boolean
  }

  // Fig 2.9 Unbalanced Set (we use scala `Ordered` typeclass directly)
  class UnbalancedSet[+Elem: Ordered](given SSet[UnbalancedSet]) {
  
  }
  
  object UnbalancedSet {
    enum Tree[+Elem] {
      case E
      case T(l: Tree[Elem], e: Elem, r: Tree[Elem])
    }
  }

  given SSet[UnbalancedSet] {
    
  }
}