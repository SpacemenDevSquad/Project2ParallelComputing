import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParIterable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.io.StdIn.readLine
import scala.util.Random
import scala.collection.mutable

/* RUNTIME */
@main def simulation(): Unit = {

  // Rock-Paper-Scissors
  println("===== Rock-Paper-Scissors =====")
  val inputSizeRPS = 5000000
  val inputRPS = functionGeneration(inputGenerationRPS, inputSizeRPS)

  print("Press Enter to Start (Type skip to skip): ")
  val RPSGo = readLine()
  println

  if RPSGo != "skip" then {
    val RPSSeq = timeTaken[Int, Boolean](MonteCarloSeq)(winningRockPaperScissors, inputRPS)
    val RPSPar = timeTaken[Int, Boolean](MonteCarloPar)(winningRockPaperScissors, inputRPS)

    println("Sequential Time Taken: " + RPSSeq._1)
    println("Parallel Time Taken: " + RPSPar._1)

    println(RPSSeq._2.getOrElse(true, 0).toDouble / (RPSSeq._2.getOrElse(true, 0) + RPSSeq._2.getOrElse(false, 1)))
    println(RPSPar._2.getOrElse(true, 0).toDouble / (RPSPar._2.getOrElse(true, 0) + RPSPar._2.getOrElse(false, 1)))

    println
    println
  }
  // Area
  println("===== Area Estimation =====")
  val inputSizeArea = 5000000
  val inputArea = functionGeneration(inputGenerationArea, inputSizeArea)

  print("Press Enter to Start (Type skip to skip): ")
  val AreaGo = readLine()
  println

  if AreaGo != "skip" then {
    println("===== Quarter Circle =====")
    val AreaQCSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimateQuarterCircle, inputArea)
    val AreaQCPar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimateQuarterCircle, inputArea)

    println("Sequential Time Taken: " + AreaQCSeq._1)
    println("Parallel Time Taken: " + AreaQCPar._1)

    println(AreaQCSeq._2.getOrElse(true, 0).toDouble / (AreaQCSeq._2.getOrElse(true, 0) + AreaQCSeq._2.getOrElse(false, 1)))
    println(AreaQCPar._2.getOrElse(true, 0).toDouble / (AreaQCPar._2.getOrElse(true, 0) + AreaQCPar._2.getOrElse(false, 1)))

    println

    println("===== Log =====")
    val AreaLogSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimateLog, inputArea)
    val AreaLogPar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimateLog, inputArea)

    println("Sequential Time Taken: " + AreaLogSeq._1)
    println("Parallel Time Taken: " + AreaLogPar._1)

    println(AreaLogSeq._2.getOrElse(true, 0).toDouble / (AreaLogSeq._2.getOrElse(true, 0) + AreaLogSeq._2.getOrElse(false, 1)))
    println(AreaLogPar._2.getOrElse(true, 0).toDouble / (AreaLogPar._2.getOrElse(true, 0) + AreaLogPar._2.getOrElse(false, 1)))

    println

    println("===== Sine =====")
    val AreaSineSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimateSine, inputArea)
    val AreaSinePar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimateSine, inputArea)

    println("Sequential Time Taken: " + AreaSineSeq._1)
    println("Parallel Time Taken: " + AreaSinePar._1)

    println(AreaSineSeq._2.getOrElse(true, 0).toDouble / (AreaSineSeq._2.getOrElse(true, 0) + AreaSineSeq._2.getOrElse(false, 1)))
    println(AreaSinePar._2.getOrElse(true, 0).toDouble / (AreaSinePar._2.getOrElse(true, 0) + AreaSinePar._2.getOrElse(false, 1)))

    println("===== Pi =====")
    val PiValSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimatePi, functionGeneration(inputGenerationArea, inputSizeRPS))
    val PiValPar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimatePi, functionGeneration(inputGenerationArea, inputSizeRPS))

    println("Sequential Time Taken: " + PiValSeq._1)
    println("Parallel Time Taken: " + PiValPar._1)

    println(4 * PiValSeq._2.getOrElse(true, 0).toDouble / (PiValSeq._2.getOrElse(true, 0) + PiValSeq._2.getOrElse(false, 1)))
    println(4 * PiValPar._2.getOrElse(true, 0).toDouble / (PiValPar._2.getOrElse(true, 0) + PiValPar._2.getOrElse(false, 1)))

    println
    println
  }

  println("===== Texas Hold-Em =====")
  val inputSizeTHE = 10000
  val numOpponentsTHE = 4
  val playerCards = List[PlayingCard](new PlayingCard(1, 2), new PlayingCard(2, 13))
  val inputTHE = functionGeneration(inputGenerationTHE(playerCards), inputSizeTHE)

  print("Press Enter to Start (Type skip to skip): ")
  val TexasGo = readLine()
  println

  if TexasGo != "skip" then {
    val THESeq = timeTaken[Iterable[PlayingCard], Boolean](MonteCarloSeq)(winningTexasHoldEmHand(numOpponentsTHE), inputTHE)
    val THEPar = timeTaken[Iterable[PlayingCard], Boolean](MonteCarloPar)(winningTexasHoldEmHand(numOpponentsTHE), inputTHE)

    println("Sequential Time Taken: " + THESeq._1)
    println("Parallel Time Taken: " + THEPar._1)

    println(THESeq._2.getOrElse(true, 0).toDouble / (THESeq._2.getOrElse(true, 0) + THESeq._2.getOrElse(false, 1)))
    println(THEPar._2.getOrElse(true, 0).toDouble / (THEPar._2.getOrElse(true, 0) + THEPar._2.getOrElse(false, 1)))

    println
    println
  }
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

// Future-Based Parallel Operation
/* def MonteCarloFut[A, B](f: A => B, input: Iterable[A]): Map[B, Int] = {
  val allFutures = Future.sequence((for item <- input yield Future{ f(item) }).toList).map(list => list.groupBy(identity[B]).map((value: B, frequency: Iterable[B]) => (value, frequency.size)))

  Await.ready(allFutures, Duration.Inf).value.get match {
    case Failure(exception) => throw exception
    case Success(results) => results
  }
} */


/* OPERATIONS */
// Texas Hold-Em-Ish
def inputGenerationTHE(hand: Iterable[PlayingCard]): Iterable[PlayingCard] = hand

def winningTexasHoldEmHand(numOpponents: Int)(hand: Iterable[PlayingCard]): Boolean = {
  var gameDeck = generateDeck(hand)

  val opponentHands = for opponent <- 1 to numOpponents yield {
    val results = draw(2, gameDeck)
    gameDeck = results._2
    results._1
  }
  val communalCards = {
    val results = draw(5, gameDeck)
    gameDeck = results._2
    results._1
  }

  val playerScore = highestCombination(hand.concat(communalCards))
  (for opponentHand <- opponentHands yield {
    highestCombination(opponentHand.concat(communalCards)) <= playerScore
  }).reduce(_&&_)
}

def generateDeck(hand: Iterable[PlayingCard]): Iterable[PlayingCard] = {
  (for { i <- 1 to 4; j <- 1 to 13 if !hand.toList.contains(new PlayingCard(i, j))} yield new PlayingCard(i, j)).toList
}

def draw(card: Int, deck: Iterable[PlayingCard]): (Iterable[PlayingCard], Iterable[PlayingCard]) = {
  val mutDeck = deck.to(mutable.ListBuffer)
  val mutHand = new mutable.ListBuffer[PlayingCard]()

  for item <- 1 to card do {
    val index = Random.nextInt(mutDeck.size)
    mutHand.addOne(mutDeck(index))
    mutDeck.remove(index)
  }

  (mutHand.toList, mutDeck.toList)
}

def highestCombination(hand: Iterable[PlayingCard]): Int = {
  if findStraightFlush(hand) then 9
  else if findFour(hand) then 8
  else if findFullHouse(hand) then 7
  else if findFlush(hand) then 6
  else if findStraight(hand) then 5
  else if findTrio(hand) then 4
  else if findTwoPair(hand) then 3
  else if findPair(hand) then 2
  else 1
}

def findStraightFlush(hand: Iterable[PlayingCard]): Boolean = {
  // TODO: Proper Implementation
  findStraight(hand) && findFlush(hand)
}

def findFour(hand: Iterable[PlayingCard]): Boolean = {
  hand.groupBy(_.faceValue).map(_._2.size).toList.contains(4)
}

def findFullHouse(hand: Iterable[PlayingCard]): Boolean = {
  val quantities = hand.groupBy(_.faceValue).map(_._2.size).toList
  quantities.contains(3) && quantities.contains(2)
}

def findFlush(hand: Iterable[PlayingCard]): Boolean = {
  hand.groupBy(_.suit).map(_._2.size).max >= 5
}

def findStraight(hand: Iterable[PlayingCard]): Boolean = {
  val faceValues = hand.map(_.faceValue).toSet
  (for faceValue <- faceValues yield {
    faceValues.contains(faceValue + 1) &&
      faceValues.contains(faceValue + 2) &&
      faceValues.contains(faceValue + 3) &&
      faceValues.contains(faceValue + 4)
  }).reduce(_||_)
}

def findTrio(hand: Iterable[PlayingCard]): Boolean = {
  hand.groupBy(_.faceValue).map(_._2.size).toList.contains(3)
}

def findTwoPair(hand: Iterable[PlayingCard]): Boolean = {
  hand.groupBy(_.faceValue).map(_._2.size).toList.count(_ == 2) >= 2
}

def findPair(hand: Iterable[PlayingCard]): Boolean = {
  hand.groupBy(_.faceValue).map(_._2.size).toList.contains(2)
}

class PlayingCard(val suit: Int, val faceValue: Int) {
  override def toString: String = "| Card: " + suit + " " + faceValue + " |"
}


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

val estimatePi = (point: (Double, Double)) => (Math.sqrt(Math.pow(0.5, 2) - Math.pow(point._1 - 0.5, 2)) + 0.5) >= point._2 &&
  (-Math.sqrt(Math.pow(0.5, 2)-Math.pow(point._1 - 0.5, 2)) + 0.5) <= point._2


/* UTILITY */
def timeTaken[A, B](monteCarlo: (A => B, Iterable[A]) => Map[B, Int])(function: A => B, input: Iterable[A]): (Double, Map[B, Int]) = {
  val startTime = System.currentTimeMillis()
  val result = monteCarlo(function, input)
  val endTime = System.currentTimeMillis()

  (endTime - startTime, result)
}

def functionGeneration[A](input: => A, times: Int): List[A] = (for i <- 1 to times yield input).toList