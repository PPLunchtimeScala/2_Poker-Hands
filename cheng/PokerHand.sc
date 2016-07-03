/*
* Distinguish the outcome by feature pattern:
*---all possible combinations for 2 distinct value------------
* e.g. Four of a kind: Pattern aaaab (2 distinct value, most frequent appearance is 4)
* e.g. Full House:     Pattern aaabb (2 distinct value, most frequent appearance is 3)
*
*
* ---all combinations for 3 distinct value--------------------
* e.g. Two Pair:      Pattern: aabbc
* e.g. Three of a kind: Pattern: aaabc
*
*
*  ---all combination for 4 distinct value--------------------
* e.g. One pair:     Pattern:aabcd
*
*
* ---all combination for 5 distinct value--------------------
* e.g. Straight         <---|
*      Flush Straight   <---| same in pattern
*      High Card          <---|
*      Garbage            <---| same in pattern
* */

//card model
sealed trait ValidatableCard
case class Card(abbr:String,suitName:String,value:Int,literal:String) extends ValidatableCard
case class InvalidCard(abbr:String) extends ValidatableCard  //Not required

//hand model
trait ValidatableHand
case class Hand(cards:List[Card]) extends ValidatableHand
case class InvalidHand(reason:String) extends ValidatableHand


// results case classes
sealed trait Results {	
	def result:String 
	def toDuplex(s:String) = if (s=="Six") s+"es" else s+"s"
}
case class FourOfAKind(mostFrequent: Card) extends Results { def result = s"Four of a Kind: ${toDuplex(mostFrequent.literal)}" }
case class ThreeOfAKind(mostFrequent: Card) extends Results { def result = s"Three of a Kind: ${toDuplex(mostFrequent.literal)}" }
case class FullHouse(distinct:List[Card])  extends Results {	def result = s"Full House: ${toDuplex(distinct.head.literal)} over ${toDuplex(distinct.last.literal)}" }
case class TwoPair(sortedHand: List[Card]) extends Results {
	def result = {
		val least = sortedHand.map(_.value).groupBy(identity).minBy(_._2.size)._1
		val bigger = sortedHand.distinct.filter(_.value != least).maxBy(_.value)
		val smaller = sortedHand.distinct.filter(_.value != least).minBy(_.value)
		s"Two Pair: ${toDuplex(bigger.literal)} over ${toDuplex(smaller.literal)}"
	}
}
case class OnePair(mostFrequent: Card) extends Results { def result = s"One Pair: ${toDuplex(mostFrequent.literal)}" }
case class StraightFlush(biggest: Card,smallest:Card) extends Results { def result = s"Straight Flush: ${toDuplex(smallest.literal)} to ${toDuplex(biggest.literal)}"}
case class Straight(biggest: Card,smallest:Card) extends Results { def result = s"Straight: ${toDuplex(smallest.literal)} to ${toDuplex(biggest.literal)}"}
case class Flush(suit: String) extends Results {def result = s"Flush: $suit"}
case class HighCard(biggest: Card) extends Results { def result = s"High Card: ${biggest.literal} of ${biggest.suitName}"}


//main app
object PokerApp {
	val cardDictionary = Map("A" ->(1, "Ace"), "2" ->(2, "Two"), "3" ->(3, "Three"),
		"4" ->(4, "Four"), "5" ->(5, "Five"), "6" ->(6, "Six"),
		"7" ->(7, "Seven"), "8" ->(8, "Eight"), "9" ->(9, "Nine"),
		"10" ->(10, "Ten"), "J" ->(11, "Jack"), "Q" ->(12, "Queen"),
		"K" ->(13, "King"))

	val suitDictionary = Map("H" -> "Hearts","S" -> "Spades","C" -> "Clubs","D" -> "Diamonds")


	def validateHand(hand: List[String]): ValidatableHand = {
		val cards = hand.map(modelCard(_))
		lazy val cardAmount = cards.length
		lazy val distinctAmount = cards.distinct.length
		lazy val invalidExists = cards.collect { case card: InvalidCard => card }.nonEmpty

		if (cardAmount > 5) InvalidHand("Invalid hand: Too many cards")
		else if (cardAmount < 5) InvalidHand("Invalid hand: Too few cards")
		else if (distinctAmount < 5) {
			lazy val card = cards.collect{case a:Card=>a}.groupBy(identity).maxBy(_._2.size)._1
			InvalidHand(s"Invalid Hand: ${card.literal} of ${card.suitName} appears ${cards.count(_ == card)} times")
		}
		else if (invalidExists) InvalidHand("Invalid hand: Invalid Cards exists") // No requirement on invalid card
		else Hand(cards.collect { case a: Card => a })
	}


	def modelCard(card: String): ValidatableCard = {
		val suitName = suitDictionary.getOrElse(card.last.toString, return InvalidCard(card))
		val valueLiteral = cardDictionary.getOrElse(card.dropRight(1), return InvalidCard(card))
		Card(card.dropRight(1), suitName, valueLiteral._1, valueLiteral._2)
	}

	def showHand(hand: Hand): Results = {
		val cards = hand.cards
		val sortedHand = cards.sortWith(_.value > _.value)
		val sortedValue = sortedHand.map(_.value)
		val distinctCards = sortedHand.distinct
		val mostFrequency = sortedHand.groupBy(_.value).maxBy(_._2.size)._2.size
		lazy val flushSuit = cards.head.suitName
		lazy val biggest = sortedHand.maxBy(_.value)
		lazy val smallest = sortedHand.minBy(_.value)
		lazy val mostFrequentCard = sortedHand.groupBy(_.value).maxBy(_._2.size)._2.head
		lazy val isFlush = cards.map(_.suitName).distinct.length.equals(1)
		lazy val isStraight = (sortedValue.head to sortedValue.last by -1).toList.equals(sortedValue)

		sortedValue.distinct.length match {
			case 2 if mostFrequency == 4        => FourOfAKind(mostFrequentCard)
			case 2                              => FullHouse(distinctCards)
			case 3 if mostFrequency == 3        => ThreeOfAKind(mostFrequentCard)
			case 3                              => TwoPair(sortedHand)
			case 4                              => OnePair(mostFrequentCard)
			case 5 if (isStraight && isFlush)   => StraightFlush(biggest,smallest)
			case 5 if isFlush                   => Flush(flushSuit)
			case 5 if isStraight                => Straight(biggest,smallest)
			case _                              => HighCard(biggest)
		}
	}

	def classifyHand(hand: List[String]): String = {
		validateHand(hand) match {
			case x: InvalidHand => x.reason
			case x: Hand => showHand(x).result
			case _ => "should not match this anyway"
		}
	}
}


PokerApp.classifyHand(List("AC", "4D", "QC", "3H"))
PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D"))
PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S"))
PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S"))
PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "10S"))
PokerApp.classifyHand(List("AC", "8D", "8C", "8H", "10S"))
PokerApp.classifyHand(List("6C", "6D", "QC", "6H", "6S"))
PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "QS"))
PokerApp.classifyHand(List("5C", "9D", "5H", "9H", "9S"))
PokerApp.classifyHand(List("AH", "7H", "QH", "4H", "10H"))
PokerApp.classifyHand(List("9C", "10D", "JC", "QH", "KS"))
PokerApp.classifyHand(List("6D", "5D", "3D", "4D", "2D"))
