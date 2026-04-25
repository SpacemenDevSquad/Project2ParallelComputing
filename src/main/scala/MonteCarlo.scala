import scala.collection.parallel.CollectionConverters.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

@main def simulation(): Unit = {
  println("Lets do some Monte Carlo!!!!")
}

//Could possibly to show proportion of a given map key to all of n


/* MONTE CARLO */
// Rudimentary Sequential Operation
def MonteCarloSeq[A, B](f: A => B, input: Iterable[A]): Map[B, Int] = {
  input.map(f).groupBy(identity[B]).map((value: B, frequency: Iterable[B]) => (value, frequency.size))
}

// Rudimentary Parallel Operation
def MonteCarloPar[A, B](f: A => B, input: Iterable[A]): Map[B, Int] = {
  input.par.map(f).toList.groupBy(identity[B]).par.map((value: B, frequency: Iterable[B]) => (value, frequency.size)).seq
}


/* OPERATIONS */
// Texas Hold-Em
def winningTexasHoldEmHand(numOpponents: Int, hand: Iterable[PlayingCard]): Unit = { }
class PlayingCard(val suit: Int, val faceValue: Int)


/* UTILITY */
def timeTaken[A, B](monteCarlo: (A => B, Iterable[A]) => Map[B, Int], function: A => B, input: Iterable[A]): (Double, Map[B, Int]) = {
  val startTime = System.currentTimeMillis()
  val result = monteCarlo(function, input)
  val endTime = System.currentTimeMillis()

  (endTime - startTime, result)
}