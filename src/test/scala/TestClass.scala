import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TestClass extends AnyFlatSpec with Matchers {

  def basicFunction(i: Int): Int = i
  def modifyFunction(i: Int): Int = i - 2


  "Monte Carlo Sequential" should "return frequencies of element" in {
    MonteCarloSeq[Int, Int](basicFunction, (1, 1, 1, 1).toList) should equal (Map(1 -> 4))
    MonteCarloSeq[Int, Int](basicFunction, (1, 2, 1, 3).toList) should equal (Map(1 -> 2, 2 -> 1, 3 -> 1))
  }

  "Monte Carlo Sequential" should "return modified frequencies of element" in {
    MonteCarloSeq[Int, Int](modifyFunction, (1, 1, 1, 1).toList) should equal(Map(-1 -> 4))
    MonteCarloSeq[Int, Int](modifyFunction, (1, 2, 1, 3).toList) should equal(Map(-1 -> 2, 0 -> 1, 1 -> 1))
  }

  "Monte Carlo Parallel" should "return frequencies of element" in {
    MonteCarloPar[Int, Int](modifyFunction, (1, 1, 1, 1).toList) should equal(Map(-1 -> 4))
    MonteCarloPar[Int, Int](modifyFunction, (1, 2, 1, 3).toList) should equal(Map(-1 -> 2, 0 -> 1, 1 -> 1))
  }

  "Monte Carlo Parallel" should "return modified frequencies of element" in {
    MonteCarloPar[Int, Int](modifyFunction, (1, 1, 1, 1).toList) should equal(Map(-1 -> 4))
    MonteCarloPar[Int, Int](modifyFunction, (1, 2, 1, 3).toList) should equal(Map(-1 -> 2, 0 -> 1, 1 -> 1))
  }


}
