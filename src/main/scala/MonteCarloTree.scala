import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}

class MonteCarloTree {
  def MonteCarloTree[A, B](decision: (Double, A, B) => A, isComplete: A => Boolean, winner: (A, B) => Boolean, currMove: A, player: B, isParallel: Boolean = true, iterations: Int = 20000000): A = {

    def SeqTree(): A = {
      val decisionMap = mutable.Map[A, Double]()
      for i <- 0 until iterations do {
        val tryMove: A = decision(Random().nextDouble(), currMove, player)
        val wasSuccess: Boolean = RunScenario(tryMove)
        if wasSuccess then decisionMap(tryMove) = 1 + decisionMap.getOrElse(tryMove, 0.0)
      }
      decisionMap.maxBy(_._2)._1
    }

    def ParTree(): A = {
      val decisionFutures: List[Future[(A, Boolean)]] = (
        for i <- 0 until iterations yield {
          Future {
            val tryMove: A = decision(Random().nextDouble(), currMove, player)
            val wasSuccess: Boolean = RunScenario(tryMove)
            (tryMove, wasSuccess)
          }
      }).toList

      val seqFuture: Future[List[(A, Boolean)]] = Future.sequence(decisionFutures)
      Await.ready(seqFuture, Duration.Inf).value.get match {
        case Failure(exception) => throw exception
        case Success(answer) => answer.filter(_._2).map[A](_._1).groupBy(value => value).maxBy(_._2.size)._1
      }
    }

    @tailrec
    def RunScenario(currMove: A): Boolean = if isComplete(currMove) then winner(currMove, player) else RunScenario(decision(Random().nextDouble(), currMove, player))

    if (isParallel) ParTree() else SeqTree()
  }
}