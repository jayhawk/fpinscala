package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2) {
    (a, b) =>
      Cons(b, a)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def sum2(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, rest) => rest
    case _             => l
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, rest) => Cons(h, rest)
    case _             => l
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    def _drop(l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else {
        _drop(tail(l), n - 1)
      }
    }
    _drop(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def _drop(l: List[A], r: List[A]): List[A] = {
      if (l == Nil) r
      else
        l match {
          case Cons(head, tail) =>
            if (f(head)) _drop(tail, r) else _drop(tail, Cons(head, r))
          case _ => r
        }
    }
    _drop(l, Nil)
  }

  def init[A](l: List[A]): List[A] = {
    def _init(l: List[A], r: List[A]): List[A] = {
      if (l == Nil) r
      else
        l match {
          case Cons(_, tail) if tail == Nil => _init(Nil, r)
          case Cons(head, tail)             => _init(tail, Cons(head, r))
          case _                            => l
        }
    }
    _init(l, Nil)
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0) { (a, _) =>
    a + 1
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil        => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]()) { (a, b) =>
    Cons(b, a)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((a, b) => Cons(f(a), b))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((a, b) => append(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    @tailrec
    def zip(a: List[A], b: List[B], c: List[C]): List[C] =
      (a, b) match {
        case (Nil, _) => c
        case (_, Nil) => c
        case (Cons(h, t), Cons(h2, t2)) => {
          zip(t, t2, Cons(f(h, h2), c))
        }
      }
    reverse(zip(a, b, Nil))
  }
}
