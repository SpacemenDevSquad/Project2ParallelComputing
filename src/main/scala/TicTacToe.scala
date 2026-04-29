import scala.util.Random

val startBoard: List[String] =
  ( " ", " ", " ",
    " ", " ", " ",
    " ", " ", " " ).toList

@main def findBestStartingMove(): Unit = {
  println(MonteCarloTree[List[String], String](TicTacToeDecision, TicTacToeComplete, TicTacToeWinner, startBoard, "x"))
}

def TicTacToeDecision(rand: Double, board: List[String], player: String): List[String] = {

  def helper(r: Double, b: List[String], p: String): List[String] = {

    var chosenSpot: Int = Math.round(r * (b.count(_ == " ") - 1)).toInt
    (for i <- b.indices yield {
      if b(i) == " " then chosenSpot -= 1
      if chosenSpot == -1 then {
        chosenSpot = -2
        p
      } else b(i)
    }).toList
  }

  val otherPlayer: String = if player == "x" then "o" else "x"
  if board.count(_ == " ") > 1 then helper(Random().nextDouble(), helper(rand, board, player), otherPlayer)
  else helper(rand, board, player)
}

def TicTacToeComplete(board: List[String]): Boolean = {
  board.count(_ == " ") == 0 || TicTacToeWinner(board, "x") || TicTacToeWinner(board, "o")
}


def TicTacToeWinner(board: List[String], player: String): Boolean = {
  var isTrue = false
  for i <- 0 to 2 do {
    if board(0+i*3) == player && board(1+i*3) == player && board(2+i*3) == player then isTrue = true
    if board(i) == player && board(i+3) == player && board(i+6) == player then isTrue = true
  }
  if board.head == player && board(4) == player && board(8) == player then isTrue = true
  if board(2) == player && board(4) == player && board(6) == player then isTrue = true
  isTrue
}