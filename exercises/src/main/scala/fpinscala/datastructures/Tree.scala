package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/*
              b
            /   \
           1   /  \
              2    3
 */

object Tree {

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l).max(maximum(r))
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => 1 + l + r)

  def max2(t: Tree[Int]): Int = fold(t)(v => v)((l, r) => l.max(r))

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => 1 + l.max(r))

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {
    println(size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
    println(max2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
    println(depth2(tree))
    println(map2(tree)(_ * 2))
  }

  private val tree = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
}