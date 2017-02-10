package tperrigo
package structures

import annotation.tailrec

sealed trait LinkedList[+E] {
  def size: Int

  def map[R](f: E => R): LinkedList[R] = this match {
    case Node(head, tail) => Node(f(head), tail.map(f))
    case Empty => Empty
  }

  @tailrec final def foldLeft[B](accumulator: B)(f: (B, E) => B): B = this match {
    case Node(head, tail) => {
      val current = f(accumulator, head)
      tail.foldLeft(current)(f)
    }
    case Empty => accumulator
  }

  def foldRight[B](accumulator: B)(f: (E, B) => B): B = {
    reverse().foldLeft(accumulator)((acc, item) => f(item, acc))
  }

  def reverse(): LinkedList[E] =
    foldLeft(LinkedList[E]()) {
      (acc, item) => Node(item, acc)
    }

  def filter(f: (E) => Boolean): LinkedList[E] =
    foldRight(LinkedList[E]()) {
      (item, acc) =>
        if (f(item))
          Node(item, acc)
        else
          acc
    }

  // re-defining map in terms of combinators
  def map2[R](f: E => R): LinkedList[R] = foldRight(LinkedList[R]()) {
    (item, acc) => Node(f(item), acc)
  }

  def ::[B >: E](element: B): LinkedList[B] = Node(element, this)

  // concatenate 2 lists; because the method is right-associative, the resulting lists
  // will have the structure prefix + this
  def :::[B >: E](prefix: LinkedList[B]): LinkedList[B] = {
    @tailrec def recurse(acc: LinkedList[B], other: LinkedList[B]): LinkedList[B] = other match {
      case Node(head, tail) => recurse(head :: acc, tail)
      case Empty => acc
    }

    recurse(this, prefix.reverse)
  }

  def foreach(f: (E) => Unit): Unit = {
    @tailrec def loop(items: LinkedList[E]): Unit = {
      items match {
        case Node(head, tail) => {
          f(head)
          loop(tail)
        }
        case Empty => {}
      }
    }
  }
}

case class Node[+E](val head: E, val tail: LinkedList[E]) extends LinkedList[E] {
  def size = 1 + tail.size
}

case object Empty extends LinkedList[Nothing] {
  val size = 0
}

object LinkedList {
  def apply[E](items: E*): LinkedList[E] =
    if (items.isEmpty)
      Empty
    else
      Node(items.head, apply(items.tail: _*))

}