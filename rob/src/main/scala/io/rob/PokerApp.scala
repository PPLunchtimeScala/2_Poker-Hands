package io.rob

import io.rob.PokerApp.CardValues.CardValue
import io.rob.PokerApp.Suits.Suit

import scala.util.{Failure, Success, Try}

object PokerApp extends App {

  object CardValues extends Enumeration {
    type CardValue = Value
    val `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, J, Q, K, A = Value

    def fromString(s: String): Try[CardValue] = {
      try {
        Success(withName(s))
      } catch {
        case e:Exception => Failure(new IllegalArgumentException(s"Not a valid value: $s"))
      }
    }
  }

  object Suits extends Enumeration {
    type Suit = Value
    val CLUBS, SPADES, DIAMONDS, HEARTS = Value

    def fromString(s: String): Try[Suit] = {
      s.toUpperCase match {
        case "S"     => Success(SPADES)
        case "C"     => Success(CLUBS)
        case "H"     => Success(HEARTS)
        case "D"     => Success(DIAMONDS)
        case unknown => Failure(new IllegalArgumentException(s"Not a suit: $unknown"))
      }
    }
  }

  case class Card(value: CardValue, suit: Suit)

  def toCard(s: String): Try[Card] = {
    for {
      value <- CardValues.fromString(s.dropRight(1))
      suit  <- Suits.fromString(s.takeRight(1))
    } yield Card(value, suit)
  }

//  implicit def ordering[CardValue]: Ordering[CardValue] = new Ordering[CardValue] {
//    override def compare(x: CardValue, y: CardValue): Int = {
//
//    }
//  }

  import CardValues._
  import Suits._

  println(toCard("10C"))
  println(toCard("DV"))
  //CardValues.withName()

  case class Hand(l: List[Card])

  def addCard(h: Hand, c: Card) = ???

  println(List(toCard("10D")).contains(toCard("10D")))


  val containsSameCardValues: Int => List[Card] => Boolean =
    targetNumberOfCards => cards => cards.size == targetNumberOfCards

  val twoPair = containsSameCardValues(2)
  val threeOfAKind = containsSameCardValues(3)
  val fourOfAKind = containsSameCardValues(4)

  def highCard(cards: List[Card]): Option[Card] = {
    cards.foldLeft(Option.empty[Card])((z, b) => {
      z match {
        case None => Some(b)
        case Some(card) => if (card.value > b.value) Some(card) else Some(b)
      }
    })
  }

  def contains(cards: Map[CardValue, List[Card]])(f: List[Card] => Boolean) = {
    cards.map { case (k, v) => f(v) }.exists(_ == true)
  }

  val l1: List[Card] = List(Card(`2`, DIAMONDS), Card(`2`, HEARTS), Card(`4`, CLUBS), Card(`4`, SPADES))
  val l2: List[Card] = List(Card(`3`, DIAMONDS), Card(`3`, HEARTS), Card(`3`, CLUBS), Card(`4`, SPADES))
  val l3: List[Card] = List(Card(A, DIAMONDS), Card(`2`, HEARTS))

  List(l1, l2 , l3) foreach { l: List[Card] =>
    val cardsByValue: Map[CardValue, List[Card]] = l.groupBy(_.value)
    val cardsBySuit: Map[Suit, List[Card]]       = l.groupBy(_.suit)

    val containsTwoPair = contains(cardsByValue)(twoPair)
    val containsThreeOfAKind = contains(cardsByValue)(threeOfAKind)
    val containsFourOfAKind = contains(cardsByValue)(fourOfAKind)

    println (s"Contains Two Pair: $containsTwoPair")
    println (s"Contains Threes: $containsThreeOfAKind")
    println (s"Contains Four: $containsFourOfAKind")
    println (s"Highest Card Value: ${highCard(l)}")

  }

}
