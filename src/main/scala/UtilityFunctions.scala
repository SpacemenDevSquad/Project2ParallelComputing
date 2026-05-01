/* UTILITY */
def timeTaken[A, B](monteCarlo: (A => B, List[A]) => Map[B, Int])(function: A => B, input: List[A]): (Double, Map[B, Int]) = {
  val startTime = System.currentTimeMillis()
  val result = monteCarlo(function, input)
  val endTime = System.currentTimeMillis()

  (endTime - startTime, result)
}

def functionGeneration[A](input: => A, times: Int): List[A] = (for i <- 1 to times yield input).toList