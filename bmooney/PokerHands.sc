object PokerApp {

  type ErrDesc = String

  case class Suit(name: String)
  case class Face(rank: Int, name: String)
  case class Card(value: Face, suit: Suit)

  abstract class Hand {
    def desc: String
  }
  case class HighCard(high: Card) extends Hand {
    override def desc = s"High card: ${high.value.name} of ${high.suit.name}"
  }
  case class Pair(face: Face) extends Hand {
    override def desc = s"One Pair: ${face.name}s"
  }
  case class ThreeOfAKind(face: Face) extends Hand {
    override def desc = s"Three of a Kind: ${face.name}s"
  }
  case class FourOfAKind(face: Face) extends Hand {
    override def desc = s"Four of a Kind: ${face.name}${Pluralinator.apply(face.name)}"
  }

  val Suits: Map[String, Suit] = Map(
    "D" -> Suit("Diamonds"),
    "H" -> Suit("Hearts"),
    "C" -> Suit("Clubs"),
    "S" -> Suit("Spades"))

  val Faces: Map[String, Face] = Map(
    "A" -> Face(0, "Ace"),
    "J" -> Face(11, "Jack"),
    "Q" -> Face(12, "Queen"),
    "K" -> Face(13, "King"),
    "2" -> Face(2, "Two"),
    "3" -> Face(3, "Three"),
    "4" -> Face(4, "Four"),
    "5" -> Face(5, "Five"),
    "6" -> Face(6, "Six"),
    "7" -> Face(7, "Seven"),
    "8" -> Face(8, "Eight"),
    "9" -> Face(9, "Nine"),
    "10" -> Face(10, "Ten"))

  val Langer: Map[Int, String] = Map(
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five"
  )

  val Pluralinator: Map[String, String] = Map(
    "Six" -> "es"
  ).withDefaultValue("s")

  val HighCardMatcher: Function[List[Card], Option[Hand]] = (cards: List[Card]) => {
    cards.sortWith(_.value.rank > _.value.rank).headOption match {
      case Some(card) => Some(HighCard(card))
      case _          => None
    }
  }

  val OnePairMatcher: Function[List[Card], Option[Hand]] = (cards: List[Card]) => {
    cards.groupBy(_.value).collect { case (_, ls @ List(_,_)) => ls.head }.headOption.map(card => Pair(card.value))
  }

  val ThreeOfAKindMatcher: Function[List[Card], Option[Hand]] = (cards: List[Card]) => {
    cards.groupBy(_.value).collect { case (_, ls @ List(_,_,_)) => ls.head }.headOption.map(card => ThreeOfAKind(card.value))
  }

  val FourOfAKindMatcher: Function[List[Card], Option[Hand]] = (cards: List[Card]) => {
    cards.groupBy(_.value).collect { case (_, ls @ List(_,_,_,_)) => ls.head }.headOption.map(card => FourOfAKind(card.value))
  }

  val HandMatchers: List[Function[List[Card], Option[Hand]]] = List(
    FourOfAKindMatcher,
    ThreeOfAKindMatcher,
    OnePairMatcher,
    HighCardMatcher
  )

  def classifyHand(hand: List[String]): String = {
    val cards: List[Card] = hand.map(toCard).collect { case Some(card) => card }

    val fail = invalidityCheck(cards)
    if (fail.nonEmpty) fail.get
    else toHand(cards) match {
      case Some(hand) => hand.desc
      case None       => "Fucked If I Know!!!"
    }
  }

  def invalidityCheck(hand: List[Card]): Option[ErrDesc] = {
    if (hand.size > 5) Some("Invalid hand: Too many cards")
    else if (hand.size < 5) Some("Invalid hand: Too few cards")
    else {
      val dupes = checkForDuplciates(hand)
      if (dupes.size > 0) Some(s"Invalid hand:${generateErrMessageForDupes(dupes)}")
      else None
    }
  }

  def checkForDuplciates(hand: List[Card]): List[Tuple2[Card, Int]] = {
    hand.groupBy(identity).collect { case (x, ls @ List(_,_,_*)) => x -> ls.size } toList
  }

  def generateErrMessageForDupes(dupes: List[Tuple2[Card, Int]]): String = dupes match {
    case Nil        => ""
    case head::tail =>
      s" ${head._1.value.name} of ${head._1.suit.name} appears ${Langer.getOrElse(head._2, "Too Many")} times${generateErrMessageForDupes(tail)}"
  }

  def toCard(card: String): Option[Card] = card match {
    case s if s.length > 3 || s.length < 2 => None
    case s if s.length == 2 => for {
        face <- Faces.get(s.substring(0, 1))
        suit <- Suits.get (s.substring (1, 2))
      } yield Card(value = face, suit = suit)
    case s if s.length == 3 => for {
      face <- Faces.get(s.substring(0, 2))
      suit <- Suits.get (s.substring (2, 3))
    } yield Card(value = face, suit = suit)
  }

  def toHand(cards: List[Card]): Option[Hand] = {
    if (cards.size != 5) None
    else HandMatchers.dropWhile(_.apply(cards).isEmpty).headOption.flatMap(_.apply(cards))
  }
}


assert(PokerApp.toCard("AC") == Some(PokerApp.Card(value = PokerApp.Face(rank = 0, name = "Ace"), suit = PokerApp.Suit(name = "Clubs"))))
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H")) == "Invalid hand: Too few cards")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D")) == "Invalid hand: Too many cards")
assert(PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S")) == "Invalid hand: Three of Hearts appears two times")
assert(PokerApp.classifyHand(List("3H", "4D", "4D", "3H", "4D")) == "Invalid hand: Three of Hearts appears two times Four of Diamonds appears three times")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")) == "High card: Queen of Clubs")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "10S")) == "One Pair: Fours")
assert(PokerApp.classifyHand(List("AC", "8D", "8C", "8H", "10S")) == "Three of a Kind: Eights")
assert(PokerApp.classifyHand(List("6C", "6D", "QC", "6H", "6S")) == "Four of a Kind: Sixes")

// Fuck it... no more time to do this stuff

assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "QS")) == "Two Pair: Queens and Fours") //Larger card listed first
assert(PokerApp.classifyHand(List("5C", "9D", "5H", "9H", "9S")) == "Full House: Nines over Fives")

assert(PokerApp.classifyHand(List("AH", "7H", "QH", "4H", "10H")) == "Flush: Hearts")
assert(PokerApp.classifyHand(List("9C", "10D", "JC", "QH", "KS")) == "Straight: Nine to King")
assert(PokerApp.classifyHand(List("6D", "5D", "3D", "4D", "2D")) == "Straight Flush: Two to Six")