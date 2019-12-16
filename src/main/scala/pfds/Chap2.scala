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
  trait SSet[A, T[A]] {
    def empty: T[A]
    def (s: T[A]) insert (x: A): T[A]
    def (s: T[A]) member (x: A): Boolean
  }

  // Fig 2.9 Unbalanced Set (used `Ordering` class directly)
  enum UnbalancedSet[+Elem] {
    case E
    case T(l: UnbalancedSet[Elem], e: Elem, r: UnbalancedSet[Elem])
  }
  
  import UnbalancedSet._
  import Ordering.Implicits.given

  given unbalancedSetImpl[Elem: Ordering]: SSet[Elem, UnbalancedSet] {
    override def empty = E
    override def (s: UnbalancedSet[Elem]) insert (x: Elem): UnbalancedSet[Elem] =
      s match {
        case E => T(E, x, E)
        case T(l, y: Elem, r) =>
          if x < y then T(l.insert(x), y, r)
          else if y < x then T(l, y, r.insert(x))
          else s
      }

    override def (s: UnbalancedSet[Elem]) member (x: Elem): Boolean =
      s match {
        case E => false
        case T(l, y: Elem, r) =>
          if x < y then l.member(x)
          else if y < x then r.member(x)
          else true
      }
  }
  
  // ex 2.2 [And91]
  def [Elem: Ordering](s: UnbalancedSet[Elem]) memberW (x: Elem): Boolean = {
    def memberWS(z: Elem, s: UnbalancedSet[Elem]): Boolean = s match {
      case E => x == z
      case T(l, y, r) => if x < y then memberWS(z, l) else memberWS(y, r)
    }
    s match {
      case E => false
      case T(_, y, _) => memberWS(y, s)
    }
  }

  // ex 2.3
  private case class InsertFail() extends Exception()
  def [Elem: Ordering](s: UnbalancedSet[Elem]) insertThrow (x: Elem): UnbalancedSet[Elem] = {
    def insertT(s: UnbalancedSet[Elem]): UnbalancedSet[Elem] = {
      s match {
        case E => T(E, x, E)
        case T(l, y: Elem, r) =>
          if (x < y) {
            val li = insertT(l)
            T(li, y, r)
          } else if (y < x) {
            val ri = insertT(r)
            T(l, y, ri)
          } else {
            throw InsertFail()
          }
      }
    }
    try {
      insertT(s)
    } catch {
      case InsertFail => s
    }
  }

  // ex 2.4
  def [Elem: Ordering](s: UnbalancedSet[Elem]) insertThrowW (x: Elem): UnbalancedSet[Elem] = {
    def insertTW(s: UnbalancedSet[Elem], z: Elem): UnbalancedSet[Elem] = {
      s match {
        case E =>
          if (x == z) throw InsertFail() else T(E, x, E)
        case T(l, y, r) =>
          if (x < y) {
            val li = insertTW(l, z)
            T(li, y, r)
          } else {
            val ri = insertTW(r, y)
            T(l, y, ri)
          }
      }
    }
    
    try {
      s match {
        case E => T(E, x, E)
        case T(_, y, _) => insertTW(s, y)
      }
    } catch {
      case InsertFail => s
    }
  }
}