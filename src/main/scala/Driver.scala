import scala.io.StdIn.readLine

@main def Driver(): Unit = {
  // Rock-Paper-Scissors
  println("===== Rock-Paper-Scissors =====")
  val inputSizeRPS = 100000
  val inputRPS = functionGeneration(inputGenerationRPS, inputSizeRPS)

  print("Press Enter to Start (Type skip to skip): ")
  val RPSGo = readLine()
  println

  if RPSGo != "skip" then {
    val RPSSeq = timeTaken[Int, Boolean](MonteCarloSeq)(winningRockPaperScissors, inputRPS)
    val RPSPar = timeTaken[Int, Boolean](MonteCarloPar)(winningRockPaperScissors, inputRPS)

    println("Sequential Time Taken: " + RPSSeq._1)
    println("Parallel Time Taken: " + RPSPar._1)

    println(RPSSeq._2.getOrElse(true, 0).toDouble / (RPSSeq._2.getOrElse(true, 0) + RPSSeq._2.getOrElse(false, 1)))
    println(RPSPar._2.getOrElse(true, 0).toDouble / (RPSPar._2.getOrElse(true, 0) + RPSPar._2.getOrElse(false, 1)))

    println
    println
  }

  // Area
  println("===== Area Estimation =====")
  val inputSizeArea = 5000000
  val inputArea = functionGeneration(inputGenerationArea, inputSizeArea)

  print("Press Enter to Start (Type skip to skip): ")
  val AreaGo = readLine()
  println

  if AreaGo != "skip" then {
    println("===== Quarter Circle =====")
    val AreaQCSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimateQuarterCircle, inputArea)
    val AreaQCPar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimateQuarterCircle, inputArea)

    println("Sequential Time Taken: " + AreaQCSeq._1)
    println("Parallel Time Taken: " + AreaQCPar._1)

    println(AreaQCSeq._2.getOrElse(true, 0).toDouble / (AreaQCSeq._2.getOrElse(true, 0) + AreaQCSeq._2.getOrElse(false, 1)))
    println(AreaQCPar._2.getOrElse(true, 0).toDouble / (AreaQCPar._2.getOrElse(true, 0) + AreaQCPar._2.getOrElse(false, 1)))

    println

    println("===== Log =====")
    val AreaLogSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimateLog, inputArea)
    val AreaLogPar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimateLog, inputArea)

    println("Sequential Time Taken: " + AreaLogSeq._1)
    println("Parallel Time Taken: " + AreaLogPar._1)

    println(AreaLogSeq._2.getOrElse(true, 0).toDouble / (AreaLogSeq._2.getOrElse(true, 0) + AreaLogSeq._2.getOrElse(false, 1)))
    println(AreaLogPar._2.getOrElse(true, 0).toDouble / (AreaLogPar._2.getOrElse(true, 0) + AreaLogPar._2.getOrElse(false, 1)))

    println

    println("===== Sine =====")
    val AreaSineSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimateSine, inputArea)
    val AreaSinePar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimateSine, inputArea)

    println("Sequential Time Taken: " + AreaSineSeq._1)
    println("Parallel Time Taken: " + AreaSinePar._1)

    println(AreaSineSeq._2.getOrElse(true, 0).toDouble / (AreaSineSeq._2.getOrElse(true, 0) + AreaSineSeq._2.getOrElse(false, 1)))
    println(AreaSinePar._2.getOrElse(true, 0).toDouble / (AreaSinePar._2.getOrElse(true, 0) + AreaSinePar._2.getOrElse(false, 1)))
  }

  // Pi
  println
  println("===== Pi Estimation =====")
  val inputSizePi = 5000000

  print("Press Enter to Start (Type skip to skip): ")
  val PiGo = readLine()
  println

  if PiGo != "skip" then {
    println("===== Pi =====")
    val PiValSeq = timeTaken[(Double, Double), Boolean](MonteCarloSeq)(estimatePi, functionGeneration(inputGenerationArea, inputSizePi))
    val PiValPar = timeTaken[(Double, Double), Boolean](MonteCarloPar)(estimatePi, functionGeneration(inputGenerationArea, inputSizePi))

    println("Sequential Time Taken: " + PiValSeq._1)
    println("Parallel Time Taken: " + PiValPar._1)

    println(4 * PiValSeq._2.getOrElse(true, 0).toDouble / (PiValSeq._2.getOrElse(true, 0) + PiValSeq._2.getOrElse(false, 1)))
    println(4 * PiValPar._2.getOrElse(true, 0).toDouble / (PiValPar._2.getOrElse(true, 0) + PiValPar._2.getOrElse(false, 1)))
    println
    println
  }


  // Texas Hold-Em-Ish
  println("===== Texas Hold-Em =====")
  val inputSizeTHE = 5000
  val numOpponentsTHE = 4
  val playerCards = List[PlayingCard](new PlayingCard(1, 2), new PlayingCard(2, 13))
  val inputTHE = functionGeneration(inputGenerationTHE(playerCards), inputSizeTHE)

  print("Press Enter to Start (Type skip to skip): ")
  val TexasGo = readLine()
  println

  if TexasGo != "skip" then {
    val THESeq = timeTaken[Iterable[PlayingCard], Boolean](MonteCarloSeq)(winningTexasHoldEmHand(numOpponentsTHE), inputTHE)
    val THEPar = timeTaken[Iterable[PlayingCard], Boolean](MonteCarloPar)(winningTexasHoldEmHand(numOpponentsTHE), inputTHE)

    println("Sequential Time Taken: " + THESeq._1)
    println("Parallel Time Taken: " + THEPar._1)

    println(THESeq._2.getOrElse(true, 0).toDouble / (THESeq._2.getOrElse(true, 0) + THESeq._2.getOrElse(false, 1)))
    println(THEPar._2.getOrElse(true, 0).toDouble / (THEPar._2.getOrElse(true, 0) + THEPar._2.getOrElse(false, 1)))

    println
    println
  }

  // Tic-Tac-Toe
  println("===== Tic-Tac-Toe =====")
  print("Press Enter to Start (Type skip to skip): ")
  val TicGo = readLine()
  println

  if TicGo != "skip" then {
    val inputSizeTTT = 1000000
    winningTTT(inputSizeTTT)("X", new TTTBoard((" ", " ", " ", " ", " ", " ", " ", " ", " ").toList))
  }
}