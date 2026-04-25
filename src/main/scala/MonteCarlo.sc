import scala.collection.parallel.CollectionConverters.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

@main def helloMonte(): Unit = {
  println("Lets do some Monte Carlo!!!!")
}

//Could possibly to show proportion of a given map key to all of n

//Parameters: function to apply to each input, iterable of inputs
// Returns: Map of outputs and the number of times each occured
object MonteCarlo  {
  def monteCarlo[A, B](f: A => B, inputs: Iterable[A]): Map[B, Int] = {


  }
}
