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
  val inputSizeRPS = 20000000

  val RPSSeq = timeTaken[Int, Boolean](MonteCarloSeq)(winningRockPaperScissors, functionGeneration(inputGenerationRPS, inputSizeRPS))
  val RPSPar = timeTaken[Int, Boolean](MonteCarloPar)(winningRockPaperScissors, functionGeneration(inputGenerationRPS, inputSizeRPS))

  println("Sequential Time Taken: " + RPSSeq._1)
  println("Parallel Time Taken: " + RPSPar._1)

  println(RPSSeq._2.getOrElse(true, 0).toDouble / (RPSSeq._2.getOrElse(true, 0) + RPSSeq._2.getOrElse(false, 1)))
  println(RPSPar._2.getOrElse(true, 0).toDouble / (RPSPar._2.getOrElse(true, 0) + RPSPar._2.getOrElse(false, 1)))


  // Area
  println("===== Area Estimation =====")
  val inputSizeArea = 20000000

  println("===== Quarter Circle =====")
  val AreaQCSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimateQuarterCircle, functionGeneration(inputGenerationArea, inputSizeRPS))
  val AreaQCPar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimateQuarterCircle, functionGeneration(inputGenerationArea, inputSizeRPS))

  println("Sequential Time Taken: " + AreaQCSeq._1)
  println("Parallel Time Taken: " + AreaQCPar._1)

  println(AreaQCSeq._2.getOrElse(true, 0).toDouble / (AreaQCSeq._2.getOrElse(true, 0) + AreaQCSeq._2.getOrElse(false, 1)))
  println(AreaQCPar._2.getOrElse(true, 0).toDouble / (AreaQCPar._2.getOrElse(true, 0) + AreaQCPar._2.getOrElse(false, 1)))

  println("===== Log =====")
  val AreaLogSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimateLog, functionGeneration(inputGenerationArea, inputSizeRPS))
  val AreaLogPar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimateLog, functionGeneration(inputGenerationArea, inputSizeRPS))

  println("Sequential Time Taken: " + AreaLogSeq._1)
  println("Parallel Time Taken: " + AreaLogPar._1)

  println(AreaLogSeq._2.getOrElse(true, 0).toDouble / (AreaLogSeq._2.getOrElse(true, 0) + AreaLogSeq._2.getOrElse(false, 1)))
  println(AreaLogPar._2.getOrElse(true, 0).toDouble / (AreaLogPar._2.getOrElse(true, 0) + AreaLogPar._2.getOrElse(false, 1)))

  println("===== Sine =====")
  val AreaSineSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimateSine, functionGeneration(inputGenerationArea, inputSizeRPS))
  val AreaSinePar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimateSine, functionGeneration(inputGenerationArea, inputSizeRPS))

  println("Sequential Time Taken: " + AreaSineSeq._1)
  println("Parallel Time Taken: " + AreaSinePar._1)

  println(AreaSineSeq._2.getOrElse(true, 0).toDouble / (AreaSineSeq._2.getOrElse(true, 0) + AreaSineSeq._2.getOrElse(false, 1)))
  println(AreaSinePar._2.getOrElse(true, 0).toDouble / (AreaSinePar._2.getOrElse(true, 0) + AreaSinePar._2.getOrElse(false, 1)))
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
def inputGenerationRPS: Int = Random.nextInt(3)

def winningRockPaperScissors(move: Int): Boolean = {
  val opponentMove: Int = Random.nextInt(3)

  move match {
    case 0 => if opponentMove == 1 then true else false
    case 1 => if opponentMove == 2 then true else false
    case 2 => if opponentMove == 0 then true else false
    case _ => false
  }
}


// Estimating Area (Domain: [(0, 0), (1, 1)])
def inputGenerationArea: (Double, Double) = (Random.nextDouble(), Random.nextDouble())

def estimateArea(function: Double => Double)(point: (Double, Double)): Boolean = Math.abs(function(point._1)) > point._2

val estimateQuarterCircle = estimateArea((x: Double) => Math.sqrt(1 - Math.pow(x, 2)))

val estimateLog = estimateArea((x: Double) => Math.log(x))

val estimateSine = estimateArea((x: Double) => Math.sin(x * (Math.PI / 2)))


/* UTILITY */
def timeTaken[A, B](monteCarlo: (A => B, Iterable[A]) => Map[B, Int])(function: A => B, input: Iterable[A]): (Double, Map[B, Int]) = {
  val startTime = System.currentTimeMillis()
  val result = monteCarlo(function, input)
  val endTime = System.currentTimeMillis()

  (endTime - startTime, result)
}

def functionGeneration[A](input: => A, times: Int): List[A] = (for i <- 1 to times yield input).toList