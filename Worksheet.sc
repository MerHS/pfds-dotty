val foo = 32

trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

object WorkTest {
  trait SemiGroup[T] with
    def (x: T) combine (y: T): T

  trait Monoid[T] extends SemiGroup[T] with
    def unit: T

  object Monoid with
    def apply[T](given Monoid[T]) = summon[Monoid[T]]

  given Monoid[String] with
    def (x: String) combine (y: String): String = x.concat(y)
    def unit: String = ""

  given Monoid[Int] with
    def (x: Int) combine (y: Int): Int = x + y
    def unit: Int = 0

  def sum[T: Monoid](xs: List[T]): T =
      xs.foldLeft(Monoid[T].unit)(_.combine(_))

  trait Functor[F[_]] {
    def [A, B](x: F[A]) map (f: A => B): F[B]
  }
  
  trait Monad[F[_]] extends Functor[F] {
    def [A, B](x: F[A]) flatMap (f: A => F[B]): F[B]
    def [A, B](x: F[A]) map (f: A => B) = x.flatMap(f `andThen` pure)
  
    def pure[A](x: A): F[A]
  }
  
  given listMonad: Monad[List] {
    def [A, B](xs: List[A]) flatMap (f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)
  }
  
  given readerMonad[Ctx]: Monad[[X] =>> Ctx => X] {
    def [A, B](r: Ctx => A) flatMap (f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x
  }
}


def f(x: Int) = x
def g(y: Int) = y
val t = f `andThen` g