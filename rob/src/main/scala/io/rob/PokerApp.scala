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

//  println (List(J, `2`, A).sorted)

  println(toCard("10C"))
  println(toCard("DV"))
  //CardValues.withName()

  case class Hand(l: List[Card])

  def addCard(h: Hand, c: Card) = ???

  println(List(toCard("10D")).contains(toCard("10D")))
}
