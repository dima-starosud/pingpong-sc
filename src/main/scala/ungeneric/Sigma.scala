package ungeneric

import scala.language.{higherKinds, implicitConversions}

sealed trait Sigma[C[_]] {
  type Arg
  val value: Arg
  implicit val proof: C[Arg]
}

object Sigma {

  private final class Pair[C[_], T](override val value: T, override val proof: C[T]) extends Sigma[C] {
    override type Arg = T
  }

  implicit def toSigma[C[_], T](x: T)(implicit p: C[T]): Sigma[C] = new Pair(x, p)
}
