package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures.
  // This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait.
  // It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`,
  // applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation
  // that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def sortPar1(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = fork {
    ps.foldRight(
      unit(List[A]()))(
      (pa, pla) => map2(pa, pla)((a, la) => a :: la))
  }

  def sequence_match[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequence_match(t)))(_ :: _)
    }

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (ps.isEmpty) lazyUnit(ps)
    else if (ps.length == 1) lazyUnit(ps)
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    as.foldRight[Par[List[A]]](unit(List[A]()))(
      (a, l) => lazyUnit(if (f(a)) map2(unit(List(a)), l)(_ ++ _) else l)
    )
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

  private case class Map2Fut[A, B, C](a: Future[A],
                                      b: Future[B],
                                      f: (A, B) => C) extends Future[C] {
    @volatile
    var cache: Option[C] = None

    def cancel(mayInterruptIfRunning: Boolean): Boolean =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    def isCancelled: Boolean = a.isCancelled || b.isCancelled

    def isDone: Boolean = cache.isDefined

    def get(): C = compute(Long.MaxValue)

    def get(timeout: Long, unit: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutns: Long): C = {
      cache match {
        case Some(c) => c
        case _ =>
          val aStart = System.nanoTime()
          val ar = a.get(timeoutns, TimeUnit.NANOSECONDS)
          val aStop = System.nanoTime()
          val aDur = aStop - aStart
          val br = b.get(timeoutns - aDur, TimeUnit.NANOSECONDS)
          val res = f(ar, br)
          cache = Some(res)
          res
      }
    }
  }

//  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
//    (es: ExecutorService) => {
//      val af = a(es)
//      val bf = b(es)
//      Map2Fut(af, bf, f)
//    }

  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3)
    println(
      run(Executors.newFixedThreadPool(5))(parFilter(l)(a => {
        println(Thread.currentThread().getId)
        a <= 2
      }))
    )
  }
}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
