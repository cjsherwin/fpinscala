package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.RNG.Simple

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  //  def &&(p: Prop): Prop = new Prop {
  //    def check: Boolean = Prop.this.check && p.check
  //  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)
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

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    Gen.choose(1, 100).flatMap(
      res => {
        val decider = res / 100d
        val weight1 = g1._2 / (g1._2 + g2._2 )
        val weight2 = g2._2 / (g1._2 + g2._2 )
        if (decider)
      }
    )

//trait Gen[A] {
//  def map[A, B](f: A => B): Gen[B] = ???
//
//  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

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
  }
}
