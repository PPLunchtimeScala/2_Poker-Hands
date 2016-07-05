//Domain objects
//
// Suits - all singleton objects
sealed trait Suit { def description: String; }
sealed trait ValidSuit extends Suit
case object Diamonds extends ValidSuit { val description = "Diamonds" }
case object Hearts extends ValidSuit { val description = "Hearts" }
case object Clubs extends ValidSuit { val description = "Clubs" }
case object Spades extends ValidSuit { val description = "Spades" }
case object InvalidSuit extends Suit { def description = "Invalid Suit" }
// Values - all singleton objects
sealed trait Value { def value: Int; def description: String }
sealed trait ValidValue extends Value
case object Ace extends ValidValue { val value = 1; val description = "Ace" }
case object Two extends ValidValue { val value = 2; val description = "Two" }
case object Three extends ValidValue { val value = 3; val description = "Three" }
case object Four extends ValidValue { val value = 4; val description = "Four" }
case object Five extends ValidValue { val value = 5; val description = "Five" }
case object Six extends ValidValue { val value = 6; val description = "Six" }
case object Seven extends ValidValue { val value = 7; val description = "Seven" }
case object Eight extends ValidValue { val value = 8; val description = "Eight" }
case object Nine extends ValidValue { val value = 9; val description = "Nine" }
case object Ten extends ValidValue { val value = 10; val description = "Ten" }
case object Jack extends ValidValue { val value = 11; val description = "Jack" }
case object Queen extends ValidValue { val value = 12; val description = "Queen" }
case object King extends ValidValue { val value = 13; val description = "King" }
case object InvalidValue extends Value { val value = 0; val description = "Invalid Value" }
// Card class to represent a card
sealed trait Card { val description: String; val value: Value; val suit: Suit }
case class ValidCard(value: Value, suit: Suit) extends Card {
  val description = value.description + " of " + suit.description
}
case object InvalidCard extends Card {
  val value = InvalidValue
  val suit = InvalidSuit
  val description = "Invalid Card"
}
//
//Classes representing poker outcomes
sealed trait Outcome { def description: String }
case class InvalidOutcome(description: String) extends Outcome {

}
case class HighCard(cards: List[Card]) extends Outcome {
  lazy val highCard: Card = {
    cards.fold(cards.head)((highCard, card) =>
      if (card.value.value > highCard.value.value) card else highCard)
  }
  def description = "High card: " + highCard.description
}
case class Pair(cards: List[Card]) extends Outcome {
  lazy val pair: Option[Card] = {
    def duplicateValues(nextCard: Card, remainingCards: List[Card]): Option[Card] = {
      remainingCards match {
        case Nil => None
        case list if list.length == 1 => None
        case list if list.contains(nextCard) => Some(nextCard)
        case list => duplicateValues(list.head, list.tail)
      }
    }
    duplicateValues(cards.head, cards.tail)
  }
  def description = "One pair: " + pair.description.pluralise
}


  //case class TwoPair(cards: List(Card)) extends Outcome
  //case class ThreeOfAKind(cards: List(Card)) extends Outcome
  //case class FourOfAKind(cards: List(Card)) extends Outcome
  //case class FullHouse(cards: List(Card)) extends Outcome
  //case class Flush(cards: List(Card)) extends Outcome
  //case class Straight(cards: List(Card)) extends Outcome
  //case class StraightFlush(cards: List(Card)) extends Outcome

  //Hand class to represent a hand
  case class Hand(cards: List[Card]) {
    lazy val duplicateCard: Option[Card] = {
      def duplicateCard(nextCard: Card, remainingCards: List[Card]): Option[Card] = {
        remainingCards match {
          case Nil => None
          case list if list.length == 1 => None
          case list if list.contains(nextCard) => Some(nextCard)
          case list => duplicateCard(list.head, list.tail)
        }
      }
      duplicateCard(cards.head, cards.tail)
    }
    val outcome: Option[Outcome] = {
      cards match {
        //Invalid outcomes
        case list if list.length < 5 => Some(InvalidOutcome("Invalid hand: Too few cards"))
        case list if list.length > 5 => Some(InvalidOutcome("Invalid hand: Too many cards"))
        case list if list.contains(InvalidCard) => Some(InvalidOutcome("Invalid hand: Contains invalid card"))
        case _ if (duplicateCard != None) => Some(InvalidOutcome(s"Invalid hand: ${duplicateCard.get.description} appears two times"))
        //Valid outcomes
        case list => if
        case list => Some(HighCard(list))
      }
    }
    def evaluateHand: Outcome = {
      outcome.get
    }
  }
  object PokerApp {
    def classifyHand(cardsAsStrings: List[String]) = {
      //Convert to cards
      val cards = cardsAsStrings.foldLeft(List.empty[Card])((cards, card) => cards :+ (card.toCard))
      //Get hand from cards
      val hand = Hand(cards)
      //Return message
      hand.evaluateHand.description
    }
  }
  println("******Hand asserts*****")
  //Basic
  assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H")) == "Invalid hand: Too few cards")
  assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D")) == "Invalid hand: Too many cards")
  assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "4F")) == "Invalid hand: Contains invalid card")
  assert(PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S")) == "Invalid hand: Three of Hearts appears two times")
  assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")) == "High card: Queen of Clubs")
  //Intermediate
  assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "10S")) == "One Pair: Fours")
  //assert(PokerApp.classifyHand(List("AC", "8D", "8C", "8H", "10S")) == "Three of a Kind: Eights")
  //assert(PokerApp.classifyHand(List("6C", "6D", "QC", "6H", "6S")) == "Four of a Kind: Sixes")
  //assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "QS")) == "Two Pair: Queens and Fours")
  // Larger card listed first
  //assert(PokerApp.classifyHand(List("5C", "9D", "5H", "9H", "9S")) == "Full House: Nines over Fives")
  // Larger card listed first
  //Advanced
  //assert(PokerApp.classifyHand(List("AH", "7H", "QH", "4H", "10H")) == "Flush: Hearts")
  //assert(PokerApp.classifyHand(List("9C", "10D", "JC", "QH", "KS")) == "Straight: Nine to King")
  //assert(PokerApp.classifyHand(List("6D", "5D", "3D", "4D", "2D")) == "Straight Flush: Two to Six")


  ////// String to Value/Suit/Card utils
  implicit class StringToCard(asString: String) {
    def toCard(): Card = {
      val (value: Value, suit: Suit) = {
        if (asString.length == 2) (asString.charAt(0).toString.toValue, asString.charAt(1).toString.toSuit)
        else if (asString.length == 3) (asString.substring(0, 2).toValue, asString.charAt(2).toString.toSuit)
        else (InvalidValue, InvalidSuit)
      }
      (value, suit) match {
        case (v:ValidValue, s:ValidSuit) => ValidCard(v, s)
        case (_,_) => InvalidCard
      }
    }
    def toSuit(): Suit = {
      asString match {
        case "D" => Diamonds
        case "H" => Hearts
        case "C" => Clubs
        case "S" => Spades
        case _ => InvalidSuit
      }
    }
    def toValue(): Value = {
      val mapping: Map[String, Value] = Map("A" -> Ace,
        "2" -> Two, "3" -> Three, "4" -> Four, "5" -> Five, "6" -> Six,
        "7" -> Seven, "8" -> Eight, "9" -> Nine, "10" -> Ten,
        "J" -> Jack, "Q" -> Queen, "K" -> King)
      mapping.getOrElse(asString, InvalidValue)
    }
    def pluralise(): String = {
      asString match {
        case "Six" => "Sixes"
        case "six" => "sixes"
        case str:String => str + "s"
    }
    }
  }
  println("Card asserts")
  assert("1".toCard()==InvalidCard)
  assert("10CC".toCard()==InvalidCard)
  assert("10F".toCard()==InvalidCard)
  assert("10D".toCard()==ValidCard(Ten, Diamonds))
  assert("AD".toCard()==ValidCard(Ace, Diamonds))