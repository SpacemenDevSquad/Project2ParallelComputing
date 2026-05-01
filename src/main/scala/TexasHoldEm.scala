import scala.util.Random
import scala.collection.mutable


/* OPERATIONS */
// Texas Hold-Em-Ish
def inputGenerationTHE(hand: Iterable[PlayingCard]): Iterable[PlayingCard] = hand

def winningTexasHoldEmHand(numOpponents: Int)(hand: Iterable[PlayingCard]): Boolean = {
  var gameDeck = generateDeck(hand)

  val opponentHands = for opponent <- 1 to numOpponents yield {
    val results = draw(2, gameDeck)
    gameDeck = results._2
    results._1
  }

  val communalCards = {
    val results = draw(5, gameDeck)
    gameDeck = results._2
    results._1
  }

  val playerScore = highestCombination(hand.concat(communalCards))

  (for opponentHand <- opponentHands yield {
    highestCombination(opponentHand.concat(communalCards)) <= playerScore
  }).reduce(_&&_)
}

def generateDeck(hand: Iterable[PlayingCard]): Iterable[PlayingCard] = {
  (for { i <- 1 to 4; j <- 1 to 13 if !hand.toList.contains(new PlayingCard(i, j))} yield new PlayingCard(i, j)).toList
}

def draw(card: Int, deck: Iterable[PlayingCard]): (Iterable[PlayingCard], Iterable[PlayingCard]) = {
  val mutDeck = deck.to(mutable.ListBuffer)
  val mutHand = new mutable.ListBuffer[PlayingCard]()

  for item <- 1 to card do {
    val index = Random.nextInt(mutDeck.size)
    mutHand.addOne(mutDeck(index))
    mutDeck.remove(index)
  }

  (mutHand.toList, mutDeck.toList)
}

def highestCombination(hand: Iterable[PlayingCard]): Int = {
  if findStraightFlush(hand) then 9
  else if findFour(hand) then 8
  else if findFullHouse(hand) then 7
  else if findFlush(hand) then 6
  else if findStraight(hand) then 5
  else if findTrio(hand) then 4
  else if findTwoPair(hand) then 3
  else if findPair(hand) then 2
  else 1
}

def findStraightFlush(hand: Iterable[PlayingCard]): Boolean = {
  findStraight(hand) && findFlush(hand)
}

def findFour(hand: Iterable[PlayingCard]): Boolean = {
  hand.groupBy(_.faceValue).map(_._2.size).toList.contains(4)
}

def findFullHouse(hand: Iterable[PlayingCard]): Boolean = {
  val quantities = hand.groupBy(_.faceValue).map(_._2.size).toList
  quantities.contains(3) && quantities.contains(2)
}

def findFlush(hand: Iterable[PlayingCard]): Boolean = {
  hand.groupBy(_.suit).map(_._2.size).max >= 5
}

def findStraight(hand: Iterable[PlayingCard]): Boolean = {
  val faceValues = hand.map(_.faceValue).toSet
  (for faceValue <- faceValues yield {
    faceValues.contains(faceValue + 1) &&
      faceValues.contains(faceValue + 2) &&
      faceValues.contains(faceValue + 3) &&
      faceValues.contains(faceValue + 4)
  }).reduce(_||_)
}

def findTrio(hand: Iterable[PlayingCard]): Boolean = {
  hand.groupBy(_.faceValue).map(_._2.size).toList.contains(3)
}

def findTwoPair(hand: Iterable[PlayingCard]): Boolean = {
  hand.groupBy(_.faceValue).map(_._2.size).toList.count(_ == 2) >= 2
}

def findPair(hand: Iterable[PlayingCard]): Boolean = {
  hand.groupBy(_.faceValue).map(_._2.size).toList.contains(2)
}

class PlayingCard(val suit: Int, val faceValue: Int) {
  override def toString: String = "| Card: " + suit + " " + faceValue + " |"
}