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

  def mapUf[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeUf(n: Int): Stream[A] =
    unfold((this, 0)) {
      case (Cons(h, t), i) if i < n => Some(h(), (t(), i + 1))
      case _ => None
    }

  def takeWhileUf(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) => if (p(h())) Some(h(), t()) else None
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    }

  def startsWith[B](s: Stream[B]): Boolean = {
    this.zipAll(s)
      .takeWhile(_._2.isDefined)
      .forAll {
        case (a, b) => a == b
      }
  }

  //  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
  //    f(z) match {
  //      case Some((a, s)) => cons(a, unfold(s)(f))
  //      case _ => empty
  //    }
  //  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h, t) => Some((cons(h(), t()), t()))
      case _ => None
    }
  }

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails.exists(_.startsWith(s))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    val out: (Stream[B], B) = foldRight((cons(z, empty), z))(
      (a, b) => {
        lazy val b1 = b
        val newB: B = f(a, b1._2)
        (cons(newB, b1._1), newB)
      })
    out._1
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toString: String =
    s"${h().toString}${if (t() != Empty) s", ${t().toString}" else ""}"
}

object Stream {
  def main(args: Array[String]): Unit = {
    val c = cons(1, cons(2, cons(3, cons(4, empty))))
    val d = cons(1, cons(2, cons(3, cons(4, empty))))
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
    println(s"ones: ${ones.take(10)}")
    println(s"constant: ${constant("x").take(10)}")
    println(from(5).takeWhile(_ < 10))
    println(fibs.take(10))
    println(s"unfold fib: ${unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }.take(10)}")
    println(s"unfold from: ${unfold(5)(n => if (n < 10) Some(n, n + 1) else None)}")
    println(s"unfold constant: ${unfold("x")(n => Some(n, n)).take(10)}")
    println(s"unfold ones: ${unfold(1)(n => Some(n, n)).take(10)}")
    println(s"mapUf: ${c.map(_ * 2)}")
    println(s"takeUf: ${c.takeUf(2)}")
    println(s"takeWhileUf: ${c.takeWhileUf(_ <= 3)}")
    println(s"zipwith: ${c.zipWith(c.map(_ * 2))((_, _))}")
    println(s"zipall: ${c.zipAll(c.take(3))}")
    println(s"zipall: ${c.zipWith(c.take(3))((_, _))}")

    println(s"startsWith: ${c.startsWith(d)}")
    println(s"startsWith: ${c.startsWith(d.take(2))}")
    println(s"startsWith: ${c.startsWith(d.drop(2))}")
    println(s"tails: ${c.tails}")
    println(s"hasSub: ${c.hasSubsequence(cons(2, cons(3, empty)))}")
    println(s"hasSub: ${c.hasSubsequence(cons(2, cons(4, empty)))}")
    println(s"scanRight: ${c.take(3).scanRight(0)(_ + _)}")
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
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a + b))
    }

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty
    }
  }
}