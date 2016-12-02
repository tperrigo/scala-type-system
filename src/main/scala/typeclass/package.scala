package tperrigo

package object typeclass {
  implicit class IsEqualSyntax[A](val lhs: A) extends AnyVal {
    def =?=(rhs: A): IsEqual[A] = IsEqual(lhs, rhs)
  }
}