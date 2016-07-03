package io.rob.algebra

import io.rob.algebra.CardValues.CardValue
import io.rob.algebra.Suits.Suit

import scala.util.{Failure, Success, Try}

/**
  * Created by rob on 03/07/16.
  */
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


case class Hand(cards: List[Card])


