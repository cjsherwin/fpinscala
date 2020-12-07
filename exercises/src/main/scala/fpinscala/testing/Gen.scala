package fpinscala.testing

import fpinscala.state._
import fpinscala.testing.Prop._
import fpinscala.{laziness => FP}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

//trait Prop {
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]

//  def &&(p: Prop): Prop = new Prop {
//    def check: Boolean = Prop.this.check && p.check
//  }
//}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      this.run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case f => f
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      this.run(max, n, rng) match {
        case Passed => Passed
        case _ => p.run(max, n, rng)
      }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def forAll[A](gen: SGen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize: MaxSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take(n).map(
          i => forAll(gen(i), s"size:$i")(f)
        )
      val prop = props.map(p => Prop {
        (max, n, rng) => p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](gen: Gen[A], label: String)(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(gen)(rng).zip(FP.Stream.from(0)).take(n).map {
        case (a, i) => try {
          val res = if (f(a)) Passed else Falsified(failMsg(label, a), i)
          res
        } catch {
          case e: Exception => Falsified(exMsg(label, a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): FP.Stream[A] =
    FP.Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def failMsg[A](label: String, a: A) = {
    s"test:$label case:$a failed"
  }

  private def exMsg[A](label: String, a: A, e: Exception): String =
    s"test:$label case:$a generated an exception: ${e.getMessage}\nstack trace: \n${e.getStackTrace.mkString(("\n"))}"

  def run(prop: Prop,
          maxSize: MaxSize = 100,
          testCases: TestCases = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    prop.run(maxSize, testCases, rng) match {
      case Falsified(msg, sc) => println(s"Falsified after $sc passed tests\n$msg")
      case Passed => println(s"Passed $testCases tests")
    }
  }
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)

  def **[B](g2: Gen[B]): Gen[(A, B)] = Gen {
    sample.map2(g2.sample)((_, _))
  }

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen { n => forSize(n).map(f) }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen {
    n => {
      val genA: Gen[A] = forSize(n)
      val g: A => Gen[B] = f(_).forSize(n)
      val genB: Gen[B] = genA.flatMap(g)
      genB
    }
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen {
    n => apply(n) ** s2.forSize(n)
  }
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => g.listOfN(n)
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen {
    n =>
      val adj = if (n == 0) 1 else n
      g.listOfN(adj)
  }
}

object Gen {
  // 8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

  def tuple[A](g: Gen[A]): Gen[(A, A)] = {
    Gen(g.sample.map2(g.sample)((_, _)))
  }

  def liftOpt[A](g: Gen[A]): Gen[Option[A]] = {
    Gen(g.sample.map2(boolean.sample)((a, b) => if (b) Some(a) else None))
  }

  def get[A](g: Gen[Option[A]], default: A): Gen[A] = {
    Gen(g.sample.map(a => a.getOrElse(default)))
  }

  def char: Gen[Char] = Gen(choose(32, 127).sample.map(i => i.toChar))

  def string(length: Int): Gen[String] = Gen(listOfN(length, Gen.char).sample.map(lc => new String(lc.toArray)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen.choose(1, 100).flatMap(
      res => {
        val (gen1, wgt1) = g1
        val (gen2, wgt2) = g2
        val cutOff = 100 * wgt1 / (wgt1 + wgt2)
        //println(s"wgt1:$wgt1, wgt2:$wgt2, cutOff:$cutOff, res:$res, pick:${if (res < cutOff) "g1" else "g2"}")
        if (res < cutOff) gen1 else gen2
      }
    )
}


object Testing {
  def main(args: Array[String]): Unit = {
    def testRun[A](g: Gen[A]): A = g.sample.run(RNG.Simple(100))._1
    // 8.4
    println(s"Gen.choose(1, 10): ${testRun(Gen.choose(1, 10))}")
    // 8.5
    println(s"Gen.unit(4): ${testRun(Gen.unit(4))}")
    println(s"Gen.boolean: ${testRun(Gen.boolean)}")
    println(s"Gen.listOfN: ${testRun(Gen.listOfN(5, Gen.unit(4)))}")
    println(s"Gen.listOfN: ${testRun(Gen.listOfN(5, Gen.boolean))}")
    println(s"Gen.listOfN: ${testRun(Gen.listOfN(5, Gen.choose(1, 10)))}")
    // play
    println(s"Gen.tuple2: ${testRun(Gen.tuple(Gen.choose(1, 50)))}")
    println(s"Gen.liftOpt: ${testRun(Gen.listOfN(10, Gen.liftOpt(Gen.choose(1, 50))))}")
    println(s"Gen.get: ${testRun(Gen.listOfN(10, Gen.get(Gen.liftOpt(Gen.choose(1, 50)), -10)))}")
    println(s"Gen.char: ${testRun(Gen.listOfN(15, Gen.char))}")
    println(s"string(10): ${testRun(Gen.string(30))}")
    // 8.7
    println(s"union: ${testRun(Gen.union(Gen.choose(0, 5), Gen.choose(10, 15)).listOfN(5))}")
    // 8.8
    println(s"weighted: ${
      testRun(
        Gen.weighted(
          (Gen.choose(0, 5), 20),
          (Gen.choose(10, 15), 5)
        ).listOfN(10))
    }")

    // 8.9
    val pF = Prop.forAll(Gen.choose(1, 5), "pF")(_ < 4)
    val pT = Prop.forAll(Gen.choose(1, 5), "pT")(_ < 5)
    println(s"&&: ${(pF && pT).run(10, 10, RNG.Simple(100))}")
    println(s"||: ${(pF || pT).run(10, 10, RNG.Simple(100))}")

    // 8.12/13
    val gen = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(SGen.listOf1(gen)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    println(s"8.12 maxProp: ${Prop.run(maxProp, 100, 100, RNG.Simple(System.currentTimeMillis))}")

    // 8.14
    val sortedProp = Prop.forAll(SGen.listOf(gen)) { ns =>
      val sorted = ns.sorted
      (sorted.length == ns.length) &&
        (if (ns.length > 1)
          sorted.sliding(2).forall(b => b.head <= b.last)
        else true) &&
        sorted.forall(ns.contains) &&
        ns.forall(sorted.contains)
    }
    println(s"8.14 sortedProp: ${Prop.run(sortedProp, 100, 100, RNG.Simple(System.currentTimeMillis))}")
  }
}
