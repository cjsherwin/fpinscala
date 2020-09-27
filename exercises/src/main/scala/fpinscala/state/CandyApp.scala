package fpinscala.state

import scala.collection.immutable


object CandyApp {

  import fpinscala.state.CandyApp.StateMachine._

  def main(args: Array[String]): Unit = {
    val initState = Machine(locked = true, 3, 5)
    log(initState)
    val inputs = List(Coin, Coin, Turn, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    StateMachine.simulateMachine(inputs).run(initState)
  }

  def log(m: Machine): Machine = {
    println(m.toString)
    m
  }

  object StateMachine {

    sealed trait Input

    case object Coin extends Input

    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int)

    def update(i: Input)(s: Machine): Machine = {
      println(i.toString)
      (i, s) match {
        case (_, m) if m.candies <= 0 => m
        case (Coin, m) if m.locked => Machine(locked = false, m.candies, m.coins + 1)
        case (Turn, m) if !m.locked => Machine(locked = true, m.candies - 1, m.coins)
        case (_, m) => m
      }
    }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      val mods: List[Machine => Machine] = inputs.map(update)
      val states: List[State[Machine, Unit]] = mods.map { mod =>
        State.modify[Machine](mod.andThen(log))
      }
      for {
        _ <- State.sequence(states)
        s <- State.get
      } yield (s.candies, s.coins)
    }
  }


}
