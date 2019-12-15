object Inter {
  trait A {
    def children: List[A]
  }

  trait B {
    def children: List[B]
  }

  class C extends A with B {
    def children: List[A&B] = List()
  }

  val c = new C
  val d: List[A&B] = c.children
}