package fpinscala.datastructures

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def foldRightViaLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))

  def foldLeft3[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)((n, z) => z + n)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = {
    val temp: mutable.Buffer[A] = mutable.Buffer.empty

    def loop(il: List[A]): List[A] = {
      il match {
        case Cons(_, Nil) => il
        case Cons(h, t) =>
          temp += h
          loop(t)
      }
    }

    loop(l)
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, len) => len + 1)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((z, _) => z + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((xs, x) => Cons(x, xs))

  def append2[A](a1s: List[A], a2s: List[A]): List[A] = {
    foldLeft(reverse(a1s), a2s)((as, a1) => Cons(a1, as))
    foldRight(a1s, a2s)((a1, as) => Cons(a1, as))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]())(append2)
  }

  def add1(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((x, xs) => Cons(x + 1, xs))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((x, xs) => Cons(x.toString, xs))

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRightViaLeft(l, List[B]())((x, xs) => Cons(f(x), xs))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaLeft(as, List[A]())((x, xs) => if (f(x)) Cons(x, xs) else xs)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRightViaLeft(as, List[B]())((a, bs) => append(f(a), bs))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addInts(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addInts(t1, t2))
    }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  @tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (Nil, Nil) => true
      case (Cons(_, _), Nil) => true
      case (Nil, Cons(_, _)) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 != h2) false else startsWith(t1, t2)
    }

  @tailrec
  def hasSubsequence[A, B](sup: List[A], sub: List[B]): Boolean =
    sup match {
      case Nil => false
      case Cons(_, t) => if (startsWith(sup, sub)) true else hasSubsequence(t, sub)
    }

  def main(args: Array[String]): Unit = {
    println(hasSubsequence(List(1, 2, 3), List(1, 2, 3))) // true
    println(hasSubsequence(List(1, 2, 3), List(1, 2))) // true
    println(hasSubsequence(List(1, 2, 3), List(2, 3))) // true
    println(hasSubsequence(Nil, Nil)) // false
    println(hasSubsequence(List(1, 2, 3), Nil)) // false
    println(hasSubsequence(List(1, 2), List(1, 2, 3))) // false
    println(hasSubsequence(List(1, 2, 3), List(1, 3, 3))) // false
    println(hasSubsequence(List(1, 3), List(1, 2, 3))) // false
    println(hasSubsequence(List(1, 4), List(1, 2, 3))) // false

    //    println(List(1, 2, 3))
    //    println(reverse(List(1, 2, 3)))
    //    println(append2(List(1, 2, 3), List(7, 8, 9)))
    //    println(concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))
    //    println(add1(List(1, 2, 3)))
    //    println(doubleToString(List(1, 2, 3)))
    //    println(map(List(1, 2, 3))(_ * 3))
    //    println(flatMap(List(1, 2, 3))(i => List(i, i)))
    //    println(addInts(List(1, 2, 3), List(7, 8, 9)))
    //    println(zipWith(List(1, 2, 3), List(7, 8, 9))((_, _)))

  }
}
