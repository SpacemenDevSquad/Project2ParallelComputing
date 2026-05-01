import scala.util.Random

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