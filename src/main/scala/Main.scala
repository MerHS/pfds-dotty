import pfds.sig._
import pfds.sig.SigUtil.{_, given}

object Main {

  def main(args: Array[String]): Unit = {
    val ordInt = new Ord[Int] {
      def eq(a: Int, b: Int) = a == b
      def lt(a: Int, b: Int) = a < b
      def leq(a: Int, b: Int) = a <= b
    }
    val uset = UnbalancedSet[Int](ordInt)
    val ntree = uset.empty |> uset.insert(3) |> uset.insert(2) |> uset.insert(6)
      |> uset.insert(5) |> uset.insert(7)
    println(ntree)
    println(uset.memberW(3)(ntree))
  }
}
