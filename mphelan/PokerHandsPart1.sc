import scala.util.{Failure, Try}

///Domain objects
//
// Suits - all singleton objects
sealed trait Suit
sealed trait ValidSuit extends Suit { def description: String; }
case object Diamonds extends ValidSuit { val description = "Diamonds" }
case object Hearts extends ValidSuit { val description = "Hearts" }
case object Clubs extends ValidSuit { val description = "Clubs" }
case object Spades extends ValidSuit { val description = "Spades" }
case object InvalidSuit extends Suit
// Values - all singleton objects
sealed trait Value
sealed trait ValidValue extends Value {
  def value: Int
  def description: String
}
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
case object InvalidValue extends Value
// Card class to represent a card
sealed trait Card
case class ValidCard(value: ValidValue, suit: ValidSuit) extends Card {
  val description = value.description + " of " + suit.description
}
case object InvalidCard extends Card

//Hand class to represent a hand
case class Hand(cards: List[Card]) {
  def evaluate: Try[String] = {
    cards match {
      case h:List[Card] if h.length < 5 => Failure("Invalid hand: Too few cards")
      case h:List[Card] if h.length > 5 => Failure("Invalid hand: Too many cards")
      case h:List[Card] => "Awesome"
    }
    def handContainsDuplicates() = {

    }
  }
}


object PokerApp {
  def classifyHand(hand: List[String]) = {
    val cards = hand.foldLeft(List.empty[Card])((cards, card) => cards :+ (card.toCard))
    cards match {
      case h:List[Card] if h.length < 5 => "Invalid hand: Too few cards"
      case h:List[Card] if h.length > 5 => "Invalid hand: Too many cards"
      case h:List[Card] => "Awesome"
    }
  }
}
val hand1 = List("AC", "4D", "QC", "3H", "10S")
PokerApp.classifyHand(hand1)
println("Hand asserts")
//Basic
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H")) == "Invalid hand: Too few cards")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D")) == "Invalid hand: Too many cards")
//assert(PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S")) == "Invalid hand: Three of Hearts appears two times")
//assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")) == "High card: Queen of Clubs")

//Intermediate
//assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "10S")) == "One Pair: Fours")
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
}
println("Card asserts")
assert("1".toCard()==InvalidCard)
assert("10CC".toCard()==InvalidCard)
assert("10F".toCard()==InvalidCard)
assert("10D".toCard()==ValidCard(Ten, Diamonds))
assert("AD".toCard()==ValidCard(Ace, Diamonds))
