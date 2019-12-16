package pfds.sig 

import pfds.sig.SigUtil.given

// Fig 2.1 Stack Signature
trait SStack[A] {
  type StackA
  case class Empty() extends Exception()
  case class Subscript() extends Exception()

  val empty: StackA
  def isEmpty(s: StackA): Boolean
  def cons(x: A)(s: StackA): StackA
  def head(s: StackA): A
  def tail(s: StackA): StackA

  def (a: A) :: (s: StackA): StackA = cons(a)(s)
  def (xs: StackA) ++ (ys: StackA): StackA = 
    if isEmpty(xs) then ys else head(xs) :: (tail(xs) ++ ys)
    
  def update(s: StackA, i: Int, y: A): StackA = {
    if (isEmpty(s))
      throw Subscript()
    else if (i == 0)
      y :: tail(s)
    else
      head(s) :: update(tail(s), i-1, y)
  }
}

// Fig 2.2 Stack with default List (prevent shadowing default `List`)
trait ListStack[A] extends SStack[A] with
  type StackA = List[A]
  val empty = List[A]()
  def isEmpty(s: StackA) = s.isEmpty
  def cons(x: A)(s: StackA) = x :: s
  def head(s: StackA) = s.head
  def tail(s: StackA) = s.tail


// Fig 2.3 Custom Stack
trait CustomStack[A] extends SStack[A] {
  enum StackA {
    case NIL
    case CONS(a: A, s: StackA)
  }
  import StackA._

  val empty = NIL
  def isEmpty(s: StackA) = s match {
    case NIL => true
    case _ => false
  }
  def cons(x: A)(s: StackA) = CONS(x, s)
  def head(s: StackA) = s match {
    case NIL => throw Empty()
    case CONS(a, s) => a
  }
  def tail(s: StackA) = s match {
    case NIL => throw Empty()
    case CONS(a, s) => s
  }
}

// Fig 2.7 Set Signature
trait SSet[E] {
  type Set

  val empty: Set
  def insert(x: E)(s: Set): Set
  def member(x: E)(s: Set): Boolean
}

// Fig 2.9 Ordered Signature (prevent shadowing default `Ordered`)
trait Ord[T] {
  def eq(a: T, b: T): Boolean
  def lt(a: T, b: T): Boolean
  def leq(a: T, b: T): Boolean
}

// Functors as class. (unlike the original code, we do not use functions because of extensions like memberW)
// This will be implemented using type class in `pfds.ChapN.scala`
class UnbalancedSet[E](given ord: Ord[E]) extends SSet[E] {
  enum Tree {
    case Leaf
    case Node(left: Tree, elem: E, right: Tree)
  }
  type Set = Tree
  
  import Tree._

  val empty = Leaf
  def member(x: E)(s: Set) = s match {
    case Leaf => false
    case Node(a, y, b) =>
      if ord.lt(x, y) then member(x)(a)
      else if ord.lt(y, x) then member(x)(b)
      else true
  }

  def insert(x: E)(s: Set) = s match {
    case Leaf => Node(Leaf, x, Leaf)
    case Node(a, y, b) =>
      if ord.lt(x, y) then Node(insert(x)(a), y, b)
      else if ord.lt(y, x) then Node(a, y, insert(x)(b))
      else s
  }

  // ex 2.2 [And91]
  def memberW(x: E)(s: Set) = {
    def memberWS(z: E, s: Set): Boolean = s match {
      case Leaf => x == z
      case Node(a, y, b) =>
        if (ord.lt(x, y))
          memberWS(z, a)
        else
          memberWS(y, b)
    }
    s match {
      case Leaf => false
      case Node(_, y, _) => memberWS(y, s)
    }
  }
}

object Chap2Sig {
  val intStackFn = new ListStack[Int] {}
  private val emptyList = intStackFn.empty
  private val oneList = intStackFn.cons(1)(emptyList)
  private val twoList = intStackFn.tail(oneList)

  // ex 2.1
  def suffixes[T](l: List[T]): List[List[T]] = l match {
    case x :: xs => l :: suffixes(l.tail)
    case _ => List(l)
  }
}