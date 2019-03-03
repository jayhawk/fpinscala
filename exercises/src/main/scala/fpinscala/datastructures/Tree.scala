package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)        => 1
    case Branch(lt, rt) => 1 + size(lt) + size(rt)
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + (size(l) max size(r))
    case Leaf(_)      => 0
  }

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((a, b) => 1 + (a max b))

  def map[A](t: Tree[A])(f: A => A): Tree[A] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v)      => Leaf(f(v))
  }

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(v)      => f(v)
  }

}
