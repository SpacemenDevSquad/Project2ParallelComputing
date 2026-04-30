import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParIterable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.io.StdIn.readLine
import scala.util.Random
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
    println("RPS Sequential Start")
    val RPSSeq = timeTaken[Int, Boolean](MonteCarloSeq)(winningRockPaperScissors, inputRPS)
    println("RPS Parallel Start")
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
  }

  // Pi
  println("===== Pi Estimation =====")
  val inputSizePi = 5000000

  print("Press Enter to Start (Type skip to skip): ")
  val PiGo = readLine()
  println

  if PiGo != "skip" then {
    println("===== Pi =====")
    val PiValSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimatePi, functionGeneration(inputGenerationArea, inputSizePi))
    val PiValPar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimatePi, functionGeneration(inputGenerationArea, inputSizePi))

    println("Sequential Time Taken: " + PiValSeq._1)
    println("Parallel Time Taken: " + PiValPar._1)

    println(4 * PiValSeq._2.getOrElse(true, 0).toDouble / (PiValSeq._2.getOrElse(true, 0) + PiValSeq._2.getOrElse(false, 1)))
    println(4 * PiValPar._2.getOrElse(true, 0).toDouble / (PiValPar._2.getOrElse(true, 0) + PiValPar._2.getOrElse(false, 1)))

    println
    println
  }

  // Texas Hold-Em-Ish
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

  // Tic-Tac-Toe
  println("===== Tic-Tac-Toe =====")
  val inputSizeTTT = 1000000
  winningTTT(inputSizeTTT)("X", new TTTBoard(("X", "X", " ", "O", " ", "O", "O", " ", " ").toList))
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
def MonteCarloFut[A, B](f: A => B, input: List[A]): Map[B, Int] = {
  val numAvailableCores: Int = Runtime.getRuntime.availableProcessors() - 1
  val executorService: ExecutorService = Executors.newFixedThreadPool(numAvailableCores)
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(executorService)

  val futureArray: Array[Future[B]] = new Array[Future[B]](numAvailableCores)
  val addToArray: ListBuffer[B] = new ListBuffer[B]
  var nextInput = 0

  val allFutures = Future.sequence((for item <- input yield Future{ f(item) }).toList).map(list => list.groupBy(identity[B]).map((value: B, frequency: Iterable[B]) => (value, frequency.size)))

  def createFutureHelper(futureIndex: Int, inputIndex: Int): Unit = futureArray(futureIndex) = Future{ f(input(inputIndex)) }

  def scanThreads(): Unit = {
    for i <- futureArray.indices do {
      if futureArray(i).isCompleted then {
        val result = futureArray(i)
        createFutureHelper(i, nextInput)
        addToArray += Await.result(result, Duration.Inf)
        nextInput += 1
      }
    }
  }

  def MainThreadHelper(): Unit = {
    while (nextInput < input.size) {
      scanThreads()
    }
  }

  MainThreadHelper()
  addToArray.toList.groupBy(identity[B]).map((value: B, frequency: Iterable[B]) => (value, frequency.size))
}


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


// Tic-Tac-Toe
def inputGenerationTTT(boardState: TTTBoard): TTTBoard = boardState

def winningTTT(inputSizeTTT: Int)(player: String, boardState: TTTBoard): Unit = {
  val validMoves = findValidMoves(player, boardState)

  for validMove <- validMoves do {
    val inputTTT = functionGeneration(inputGenerationTTT(validMove), inputSizeTTT)
    val otherPlayer = if player == "X" then "O" else "X"

    println("= Option =")
    println(validMove.toString)
    println

    val TTTSeq = timeTaken[TTTBoard, Boolean](MonteCarloSeq)(exploreBoard(player)(otherPlayer), inputTTT)
    val TTTPar = timeTaken[TTTBoard, Boolean](MonteCarloPar)(exploreBoard(player)(otherPlayer), inputTTT)

    println("Sequential Time Taken: " + TTTSeq._1)
    println("Parallel Time Taken: " + TTTPar._1)

    println(TTTSeq._2.getOrElse(true, 0).toDouble / (TTTSeq._2.getOrElse(true, 0) + TTTSeq._2.getOrElse(false, 1)))
    println(TTTPar._2.getOrElse(true, 0).toDouble / (TTTPar._2.getOrElse(true, 0) + TTTPar._2.getOrElse(false, 1)))

    println
  }
}

@tailrec
def exploreBoard(player: String)(currentPlayer: String)(boardState: TTTBoard): Boolean = {
  val results = testWin(boardState)
  if results._1 then return results._2 == player

  val otherPlayer = if currentPlayer == "X" then "O" else "X"
  val validMoves = findValidMoves(otherPlayer, boardState)

  if validMoves.isEmpty then return false

  exploreBoard(player)(otherPlayer)(validMoves(Random.nextInt(validMoves.size)))
}

def findValidMoves(player: String, boardState: TTTBoard): List[TTTBoard] = {
  (for placement <- 0 until boardState.values.length if boardState.values(placement) == " " yield boardState.move(placement, player)).toList
}

def testWin(boardState: TTTBoard): (Boolean, String) = {
  if testColumns(boardState, "X") || testRows(boardState, "X") || testDiagonals(boardState, "X") then (true, "X")
  else if testColumns(boardState, "O") || testRows(boardState, "O") || testDiagonals(boardState, "O") then (true, "O")
  else (false, " ")
}

def testColumns(boardState: TTTBoard, player: String): Boolean = {
  (boardState.values(0) == player && boardState.values(3) == player && boardState.values(6) == player) ||
    (boardState.values(1) == player && boardState.values(4) == player && boardState.values(7) == player) ||
    (boardState.values(2) == player && boardState.values(5) == player && boardState.values(8) == player)
}

def testRows(boardState: TTTBoard, player: String): Boolean = {
  (boardState.values(0) == player && boardState.values(1) == player && boardState.values(2) == player) ||
    (boardState.values(3) == player && boardState.values(4) == player && boardState.values(5) == player) ||
    (boardState.values(6) == player && boardState.values(7) == player && boardState.values(8) == player)
}

def testDiagonals(boardState: TTTBoard, player: String): Boolean = {
  (boardState.values(0) == player && boardState.values(5) == player && boardState.values(8) == player) ||
    (boardState.values(2) == player && boardState.values(5) == player && boardState.values(6) == player)
}

class TTTBoard(val values: List[String]) {
  def move(placement: Int, player: String): TTTBoard = {
    val newBoardState = (for index <- 0 until values.size yield if index == placement then player else values(index)).toList
    new TTTBoard(newBoardState)
  }

  override def toString: String = (for index <- 0 until values.size yield {
    if index == 2 || index == 5 then values(index) + "\n---------\n"
    else if index == 8 then values(index) + "\n"
    else values(index) + " | "
  }).mkString
}


/* UTILITY */
def timeTaken[A, B](monteCarlo: (A => B, Iterable[A]) => Map[B, Int])(function: A => B, input: Iterable[A]): (Double, Map[B, Int]) = {
  val startTime = System.currentTimeMillis()
  val result = monteCarlo(function, input)
  val endTime = System.currentTimeMillis()

  (endTime - startTime, result)
}

def functionGeneration[A](input: => A, times: Int): List[A] = (for i <- 1 to times yield input).toList