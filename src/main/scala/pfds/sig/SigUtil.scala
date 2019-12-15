package pfds.sig 

object SigUtil {
  trait Piper {
    def [A <: AnyVal, B] (x: A) |> (f: A => B) = f(x)
  }

  given piperOps: Piper
}