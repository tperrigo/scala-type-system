package tperrigo
package typeclass

import simulacrum._

/**
 * Functor instance based on Michael Pilquist's videos on Functional Structures in Scala
 */
@typeclass trait Functor[F[_]] { self =>
  /**
   * Map is a method for transforming a value within a context (a type contructor).
   * It is the signature method for Functor, and must be implemented for any type
   * which requires a Functor instance.
   */
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // Derived methods: Given a (lawful) implementation of map, we can derive a set of useful methods for working with funtors

  def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)

  def as[A, B](fa: F[A], b: => B): F[B] = map(fa)(_ => b)

  def void[A](fa: F[A]): F[Unit] = as(fa, ())

  def compose[G[_]](implicit G: Functor[G]): Functor[λ[X => F[G[X]]]] = new Functor[λ[X => F[G[X]]]] {
    def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
      self.map(fga)(ga => G.map(ga)(a => f(a)))
  }
}

/**
 * Functor companion object.  Contains Functor instances for several common types.
 */
object Functor {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def function1Functor[X]: Functor[X => ?] = new Functor[X => ?] {
    def map[A, B](fa: X => A)(f: A => B): X => B = fa andThen f
  }
}
