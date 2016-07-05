sealed trait Suit
case object Diamonds extends Suit
case object Clubs extends Suit
case object Spades extends Suit
case object Hearts extends Suit

sealed trait HandCategory {
  def show: String = ???
}
case object StraightFlush extends HandCategory
case object FourOfKind extends HandCategory
case object FullHouse extends HandCategory
case object Flush extends HandCategory
case object Straight extends HandCategory
case object ThreeOfKind extends HandCategory
case object TwoPair extends HandCategory
case object OnePair extends HandCategory
case object HighCard extends HandCategory
case object NoCategory extends HandCategory

case class Card(value: String, suit: Suit)

object Card {
  def parse(s: String): Either[String, Card] = {
    val mayCard = for {
      value <- extractValue(s)
      charSuit <- s.lastOption
      suit <- extractSuit(charSuit)
      card = Card(value, suit)
    } yield card

    mayCard match {
      case Some(c) => Right(c)
      case None => Left(s"invalid card value $s")
    }
  }

  private def extractValue(s: String): Option[String] =
    "[2-9]|A|J|Q|K".r.findFirstIn(s)

  private def extractSuit(c: Char): Option[Suit] = c match {
    case 'D' => Some(Diamonds)
    case 'H' => Some(Hearts)
    case 'C' => Some(Clubs)
    case 'S' => Some(Spades)
    case _ => None
  }
}

case class Hand(cards: Set[Card]) {
  def category: HandCategory = ???
}


object Hand {
  def parse(stringCards: List[String]): Either[String, Hand] =
    stringCards match {
      case Nil =>
        Left("Invalid hand: Empty cards")
      case l if l.size < 5 || l.size > 5 =>
        Left("Invalid hand: You must have 5 cards")
      case l => stringCards.map(Card.parse).partition(_.isLeft) match {
        case (Nil, validCards) =>
          val cards = validCards.map(_.right.get).toSet
          if(cards.size == 5) Right(Hand(cards))
          else Left(s"Invalid hand: duplicated cards ${stringCards.mkString(", ")}")
        case (errors, _) =>
          Left(errors.map(_.left.get).mkString(", "))
      }
    }
}


object PokerApp {
  def classifyHand(cards: List[String]): String = {
    val result = for {
      hand <- Hand.parse(cards).right
      category <- classify(hand).right
    } yield category.show

    result.fold(e => e, c => c)
  }

  def classify(hand: Hand): Either[String, HandCategory] = ???
}