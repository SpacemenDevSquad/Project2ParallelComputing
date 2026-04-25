import scala.collection.parallel.CollectionConverters.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

@main def helloMonte(): Unit = {
  println("Lets do some Monte Carlo!!!!")
}
object MonteCarlo  {
  def monteCarlo[A, B](f: A => B, inputs: Iterable[A]): Map[B, Int] = {

    
  }
}
