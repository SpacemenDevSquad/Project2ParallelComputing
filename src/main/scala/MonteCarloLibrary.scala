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

/* MONTE CARLO */
// Rudimentary Sequential Operation
def MonteCarloSeq[A, B](f: A => B, input: List[A]): Map[B, Int] = {
  input.map(f).groupBy(identity[B]).map((value: B, frequency: Iterable[B]) => (value, frequency.size))
}

// Rudimentary Parallel Operation
def MonteCarloPar[A, B](f: A => B, input: List[A]): Map[B, Int] = {
  input.par.map(f).groupBy(identity[B]).map((value: B, frequency: ParIterable[B]) => (value, frequency.size)).seq
}

def MonteCarloFut[A, B](f: A => B, input: List[A]): Map[B, Int] = {
  val numAvailableCores: Int = Runtime.getRuntime.availableProcessors() - 1
  val executorService: ExecutorService = Executors.newFixedThreadPool(numAvailableCores)
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(executorService)

  val futureArray: Array[Future[B]] = new Array[Future[B]](numAvailableCores)
  val addToArray: ListBuffer[B] = new ListBuffer[B]
  var nextInput = 0
  
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

  for i <- futureArray.indices do {
    createFutureHelper(i, nextInput)
    nextInput += 1
  }
  MainThreadHelper()
  addToArray.toList.groupBy(identity[B]).map((value: B, frequency: Iterable[B]) => (value, frequency.size))
}