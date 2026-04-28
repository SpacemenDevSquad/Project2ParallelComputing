import scala.util.Random

@main def findBestStartingMove(): Unit = {

}

def TicTacToeDecision(rand: Double, board: List[String], player: String): List[String] = {

  def helper(r: Double, b: List[String], p: String): List[String] = {

    var chosenSpot: Int = Math.round(r * b.count(_ == " ")).toInt
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
  board.count(_ == " ") == 0
}

val board: List[String] =
  ( " ", " ", " ",
    " ", " ", " ",
    " ", " ", " " ).toList


def TicTacToeWinner(board: List[String], player: String): Boolean = {
  true
}