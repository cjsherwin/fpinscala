package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] =
   this match {
     case Left(e) => Left(e)
     case Right(v) => Right(f(v))
   }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
   this match {
     case Left(e) => Left(e)
     case Right(v) => f(v)
   }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
   this match {
     case Left(e) => b
     case Right(v) => Right(v)
   }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
//   this match {
//     case Left(e) => Left(e)
//     case Right(a) => b.map(f(a, _))
//   }
  for {
    a <- this
    b1 <- b
  } yield f(a, b1)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(List[B]()))(
      (a, el) => f(a).map2(el)(_ :: _)
    )

  def traverse2[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse2(t)(f))(_ :: _)
    }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(a => a)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def main(args: Array[String]): Unit = {
    println(traverse(List(1,2,3))(a => Right(a.toString)))
    println(sequence(List(Right(1), Right(2), Left("error"), Left("error 2"))))
  }
}

sealed trait Eith[+E, +A] {
  def map[B](f: A => B): Eith[E, B] =
    this match {
      case Lef(le) => Lef(le)
      case Righ(a) => Righ(f(a))
    }

//  def flatMap[B](f: A => Eith[E, B]): Eith[E, B] =
//    this match {
//      case Lef(le) => Lef(le)
//      case Righ(a) => f(a)
//    }

//  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =

}

case class Lef[+E](get: List[E]) extends Eith[E,Nothing]
case class Righ[+A](get: A) extends Eith[Nothing,A]

object Eith {
  def main(args: Array[String]): Unit = {
    println(Lef(List(1,2,3)))
    println(Righ("Hello"))
    println((Righ(23d): Eith[String, Double]).map(_.toString))
    println((Lef(List("err1", "err2")): Eith[String, Double]).map(d => d))
  }
}