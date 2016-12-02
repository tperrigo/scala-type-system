package tperrigo
package typeclass

/**
 * Simple class used for law testing
 */
case class IsEqual[A](lhs: A, rhs: A) {
  def isEqual(implicit eq: Equal[A]): Boolean = eq.equal(lhs, rhs)
}