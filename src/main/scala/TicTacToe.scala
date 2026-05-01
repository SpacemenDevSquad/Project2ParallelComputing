import scala.annotation.tailrec
import scala.util.Random

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