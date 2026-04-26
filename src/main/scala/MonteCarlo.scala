import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParIterable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

/* RUNTIME */
@main def simulation(): Unit = {
  // Rock-Paper-Scissors
  println("===== Rock-Paper-Scissors =====")
  val inputSizeRPS = 50000000

  val RPSSeq = timeTaken[Int, Boolean](MonteCarloSeq)(winningRockPaperScissors, inputGeneration(0, inputSizeRPS))
  val RPSPar = timeTaken[Int, Boolean](MonteCarloPar)(winningRockPaperScissors, inputGeneration(0, inputSizeRPS))

  println("Sequential Time Taken: " + RPSSeq._1)
  println("Parallel Time Taken: " + RPSPar._1)

  println(RPSSeq._2.getOrElse(true, 0).toDouble / (RPSSeq._2.getOrElse(true, 0) + RPSSeq._2.getOrElse(false, 1)))
  println(RPSPar._2.getOrElse(true, 0).toDouble / (RPSPar._2.getOrElse(true, 0) + RPSPar._2.getOrElse(false, 1)))
}


/* MONTE CARLO */
// Rudimentary Sequential Operation
def MonteCarloSeq[A, B](f: A => B, input: Iterable[A]): Map[B, Int] = {
  input.map(f).groupBy(identity[B]).map((value: B, frequency: Iterable[B]) => (value, frequency.size))
}

// Rudimentary Parallel Operation
def MonteCarloPar[A, B](f: A => B, input: Iterable[A]): Map[B, Int] = {
  input.par.map(f).groupBy(identity[B]).map((value: B, frequency: ParIterable[B]) => (value, frequency.size)).seq
}


/* OPERATIONS */
// Texas Hold-Em
def winningTexasHoldEmHand(numOpponents: Int)(hand: Iterable[PlayingCard]): Boolean = {
  true
}

class PlayingCard(val suit: Int, val faceValue: Int)

// Rock-Paper-Scissors
def winningRockPaperScissors(move: Int): Boolean = {
  val opponentMove: Int = Random.nextInt(3)

  move match {
    case 0 => if opponentMove == 1 then true else false
    case 1 => if opponentMove == 2 then true else false
    case 2 => if opponentMove == 0 then true else false
    case _ => false
  }
}


/* UTILITY */
def timeTaken[A, B](monteCarlo: (A => B, Iterable[A]) => Map[B, Int])(function: A => B, input: Iterable[A]): (Double, Map[B, Int]) = {
  val startTime = System.currentTimeMillis()
  val result = monteCarlo(function, input)
  val endTime = System.currentTimeMillis()

  (endTime - startTime, result)
}

def inputGeneration[A](input: A, times: Int): List[A] = (for i <- 1 to times yield input).toList