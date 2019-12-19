package net.kinetc.pfds.sig 

object SigUtil {
  trait Piper {
    def [A, B] (x: A) |> (f: A => B) = f(x)
  }

  given piperOps: Piper
}