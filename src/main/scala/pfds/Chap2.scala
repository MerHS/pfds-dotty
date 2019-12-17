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
  trait SSet[A, T[_]] {
    def empty: T[A]
    def (s: T[A]) insert (x: A): T[A]
    def (s: T[A]) member (x: A): Boolean
  }

  // Fig 2.9 Unbalanced Set (used `Ordering` class directly)
  enum Tree[+Elem] {
    case Leaf
    case Node(l: Tree[Elem], e: Elem, r: Tree[Elem])
  }
  
  import Tree._
  import Ordering.Implicits.given

  given unbalancedSetImpl[Elem: Ordering]: SSet[Elem, Tree] {
    override def empty = Leaf
    override def (s: Tree[Elem]) insert (x: Elem): Tree[Elem] =
      s match {
        case Leaf => Node(Leaf, x, Leaf)
        case Node(l, y, r) =>
          val ord = summon[Ordering[Elem]]
          if ord.lt(x, y) then Node(l.insert(x), y, r)
          else if ord.lt(y, x) then Node(l, y, r.insert(x))
          else s
      }

    override def (s: Tree[Elem]) member (x: Elem): Boolean =
      s match {
        case Leaf => false
        case Node(l, y, r) =>
          val ord = summon[Ordering[Elem]]
          if ord.lt(x, y) then l.member(x)
          else if ord.lt(y, x) then r.member(x)
          else true
      }
  }
  
  // ex 2.2 [And91]
  def [Elem: Ordering](s: Tree[Elem]) memberW (x: Elem): Boolean = {
    def memberWS(z: Elem, s: Tree[Elem]): Boolean = s match {
      case Leaf => x == z
      case Node(l, y, r) => if x < y then memberWS(z, l) else memberWS(y, r)
    }
    s match {
      case Leaf => false
      case Node(_, y, _) => memberWS(y, s)
    }
  }

  // ex 2.3
  private case class InsertFail() extends Exception()
  def [Elem: Ordering](s: Tree[Elem]) insertThrow (x: Elem): Tree[Elem] = {
    def insertT(s: Tree[Elem]): Tree[Elem] = {
      s match {
        case Leaf => Node(Leaf, x, Leaf)
        case Node(l, y, r) =>
          val ord = summon[Ordering[Elem]]
          if (ord.lt(x, y)) {
            val li = insertT(l)
            Node(li, y, r)
          } else if (ord.lt(y, x)) {
            val ri = insertT(r)
            Node(l, y, ri)
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
  def [Elem: Ordering](s: Tree[Elem]) insertThrowW (x: Elem): Tree[Elem] = {
    def insertTW(s: Tree[Elem], z: Elem): Tree[Elem] = {
      s match {
        case Leaf =>
          if (x == z) throw InsertFail() else Node(Leaf, x, Leaf)
        case Node(l, y, r) =>
          if (x < y) {
            val li = insertTW(l, z)
            Node(li, y, r)
          } else {
            val ri = insertTW(r, y)
            Node(l, y, ri)
          }
      }
    }
    
    try {
      s match {
        case Leaf => Node(Leaf, x, Leaf)
        case Node(_, y, _) => insertTW(s, y)
      }
    } catch {
      case InsertFail => s
    }
  }

  // ex 2.5
  def complete[Elem](x: Elem, d: Int): Tree[Elem] = {
    if (d <= 0) {
      Leaf
    } else {
      val child = complete(x, d - 1)
      Node(child, x, child)
    }
  }

  // n >= 3
  def create2[Elem](x:Elem, n: Int): (Tree[Elem], Tree[Elem]) = {
    if (n <= 0) {
      (Leaf, Leaf)
    } else if (n == 1) {
      (Leaf, Node(Leaf, x, Leaf))
    } else if (n % 4 == 3) { // 4k + 3 -> (k, k+1), (k+1, k+1)
      val (l, r) = create2(x, n / 2) // (k, k+1)
      return (Node(l, x, r), Node(r, x, r))
    } else { // 4k + 1 -> (k, k), (k, k+1)
      val (l, r) = create2(x, n / 2 + 1) // 2k+1
      return (Node(l, x, l), Node(l, x, r))
    }
  }

  def completeN[Elem](x: Elem, n: Int): Tree[Elem] = {
    if (n <= 0) {
      Leaf
    } else if (n == 1) {
      Node(Leaf, x, Leaf)
    } else if (n % 2 == 0) {
      val child = completeN(x, n / 2)
      Node(child, x, child) 
    } else {
      val (l, r) = create2(x, n)
      Node(l, x, r)
    }
  }

  // ex 2.6, FiniteMap 
  trait FiniteMap[K, V, T] {
    def empty: T
    def (m: T) bind (key: K, value: V): T

    @throws[FiniteMap.NotFound[K]]
    def (m: T) lookup (key: K): V
  }

  object FiniteMap {
    case class NotFound[K](value: K) extends Exception
  }

  given treeFiniteMapImpl[K: Ordering, V]: FiniteMap[K, V, Tree[(K, V)]] {
    override def empty = Leaf

    override def (m: Tree[(K, V)]) insert (key: K, value: V): Tree[(K, V)] = {
      m match {
        case Leaf => Node(Leaf, (key, value), Leaf)
        case Node(l, t@(key2, val2), r) => 
          val ord = summon[Ordering[K]]
          if ord.lt(key, key2) then Node(l.insert(key, value), t, r)
          else if ord.lt(key2, key) then Node(l, t, r.insert(key, value))
          else Node(l, (key, value), r)
      }
    }
    
    override def (m: Tree[(K, V)]) lookup(key: K): V = {
      m match {
        case Leaf => throw FiniteMap.NotFound(key)
        case Node(l, t@(key2, value), r) => 
          val ord = summon[Ordering[K]]
          if ord.lt(key, key2) then l.lookup(key)
          else if ord.lt(key2, key) then r.lookup(key)
          else value
      }
    }
  }
}