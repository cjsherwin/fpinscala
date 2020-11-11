package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toListRec: List[A] = {
    @tailrec
    def go(s: Stream[A], l: List[A]): List[A] = {
      s match {
        case Empty => l
        case Cons(h, t) => go(t(), h() :: l)
      }
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

  def takeWhileFR(p: A => Boolean): Stream[A] =
    this.foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((h, t) => p(h) && t)

  def headOption: Option[A] =
    this.foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    this.foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    this.foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](sa: => Stream[B]): Stream[B] =
    this.foldRight(sa)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toString: String =
    s"${h().toString}${if (t() != Empty) s", ${t().toString}" else ""}"
}

object Stream {
  def main(args: Array[String]): Unit = {
    val c = cons(1, cons(2, cons(3, cons(4, empty))))
    println(c.toString)
    println(c.toList)
    println(c.take(2))
    println(c.drop(1))
    println(c.takeWhile(_ <= 2))
    println(c.takeWhileFR(_ <= 2))
    println(c.forAll { i => println(s"i: $i"); i < 2 })
    //    println(List(1,2,3,4).foldRight(0){(n, s) => println(s"s: $s, n: $n"); n + s})
    //    println(c.foldRight(0){(n, s) =>
    //      println(s"s: $s, n: $n")
    //      n + s
    //    })
    println(from(5).takeWhile(_ < 10))
    println(fibs.take(10))
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int, s: Stream[Int]): Stream[Int] = {
      val next = a + b
      go(b, next, s.append(cons(next, empty)))
    }
    go(0, 1, Stream(0, 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}