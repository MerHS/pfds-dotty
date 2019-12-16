package pfds

import scala.language.implicitConversions
import scala.collection.immutable._
import scala.math.Ordered

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

  enum CustomStack[+A] {
    case NIL
    case CONS(a: A, s: CustomStack[A])
  }

  given SStack[CustomStack] {
    import CustomStack._

    override def empty[A] = NIL
    override def [A](xs: CustomStack[A]) isEmpty = xs match {
      case NIL => true
      case _ => false
    }

    override def [A, B >: A](elem: B) cons (s: CustomStack[A]) = CONS(elem, s)
    override def [A](s: CustomStack[A]) head = s match {
      case NIL => throw Empty()
      case CONS(a, s) => a
    }
    override def [A](s: CustomStack[A]) tail = s match {
      case NIL => throw Empty()
      case CONS(a, s) => s
    }
  }

  // Fig 2.7 Set type class
  trait SSet[T[_]] {
    def empty[A]: T[A]
    def [A](s: T[A]) insert (x: A): T[A]
    def [A](s: T[A]) member (x: A): Boolean
  }

  // Fig 2.9 Unbalanced Set (used `Ordering` class directly)
  enum UnbalancedSet[+Elem] {
    case E
    case T(l: UnbalancedSet[Elem], e: Elem, r: UnbalancedSet[Elem])
  }

  given SSet[UnbalancedSet] {
    import UnbalancedSet._
    import math.Ordering.Implicits.given

    override def empty[A: Ordering] = E
    override def [A: Ordering](s: UnbalancedSet[A]) insert (x: A): UnbalancedSet[A] =
      s match {
        case E => T(E, x, E)
        case T(l, y: A, r) =>
          if x < y then T(l.insert(x), y, r)
          else if y < x then T(l, y, r.insert(x))
          else s
      }

    override def [A: Ordering](s: UnbalancedSet[A]) member (x: A): Boolean =
      s match {
        case E => false
        case T(l, y: A, r) =>
          if x < y then l.member(x)
          else if y < x then r.member(x)
          else true
      }
  }
}