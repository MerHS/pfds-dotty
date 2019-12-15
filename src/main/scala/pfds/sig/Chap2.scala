package pfds.sig 

import pfds.sig.SigUtil.given

trait SStack[A] with
  type StackA
  case class Empty() extends Exception()
  val empty: StackA
  def isEmpty(s: StackA): Boolean
  def cons(x: A, s: StackA): StackA
  def head(s: StackA): A
  def tail(s: StackA): StackA


// Fig 2.2 Stack with default List (rename `List` to prevent shadowing default `List`)
trait ListStack[A] extends SStack[A] with
  type StackA = List[A]
  override val empty = List[A]()
  override def isEmpty(s: StackA) = s.isEmpty
  override def cons(x: A, s: StackA) = x :: s
  override def head(s: StackA) = s.head
  override def tail(s: StackA) = s.tail


// Fig 2.3 Custom Stack
trait CustomStack[A] extends SStack[A] {
  enum StackA {
    case NIL
    case CONS(a: A, s: StackA)
  }
  import StackA._

  override val empty = NIL
  override def isEmpty(s: StackA) = s match {
    case NIL => true
    case _ => false
  }
  override def cons(x: A, s: StackA) = CONS(x, s)
  override def head(s: StackA) = s match {
    case NIL => throw Empty()
    case CONS(a, s) => a
  }
  override def tail(s: StackA) = s match {
    case NIL => throw Empty()
    case CONS(a, s) => s
  }
}

object Chap2Sig {
  val intStackFn = new ListStack[Int] {}
  private val emptyList = intStackFn.empty
  private val oneList = intStackFn.cons(1, emptyList)
  private val twoList = intStackFn.tail(oneList)
}