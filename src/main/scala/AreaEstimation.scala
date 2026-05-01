import scala.util.Random

// Estimating Area (Domain: [(0, 0), (1, 1)])
def inputGenerationArea: (Double, Double) = (Random.nextDouble(), Random.nextDouble())

def estimateArea(function: Double => Double)(point: (Double, Double)): Boolean = Math.abs(function(point._1)) > point._2

val estimateQuarterCircle: ((Double, Double)) => Boolean = estimateArea((x: Double) => Math.sqrt(1 - Math.pow(x, 2)))

val estimateLog: ((Double, Double)) => Boolean = estimateArea((x: Double) => Math.log(x))

val estimateSine: ((Double, Double)) => Boolean = estimateArea((x: Double) => Math.sin(x * (Math.PI / 2)))

val estimatePi: ((Double, Double)) => Boolean = (point: (Double, Double)) => (Math.sqrt(Math.pow(0.5, 2) - Math.pow(point._1 - 0.5, 2)) + 0.5) >= point._2 &&
  (-Math.sqrt(Math.pow(0.5, 2)-Math.pow(point._1 - 0.5, 2)) + 0.5) <= point._2