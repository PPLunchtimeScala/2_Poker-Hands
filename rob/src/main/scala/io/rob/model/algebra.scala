package io.rob.model

import io.rob.model.CardValues.CardValue
import io.rob.model.Suits.Suit

import scalaz.{-\/, \/, \/-}


object CardValues extends Enumeration {
  type CardValue = Value
  val `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, J, Q, K, A = Value

  def fromString(s: String): String \/ CardValue = {
    try {
      \/-(withName(s))
    } catch {
      case e:Exception => -\/ (s"Not a valid value: $s")
    }
  }
}


object Suits extends Enumeration {
  type Suit = Value
  val CLUBS, SPADES, DIAMONDS, HEARTS = Value

  def fromString(s: String): String \/ Suit = {
    s.toUpperCase match {
      case "S"     => \/-(SPADES)
      case "C"     => \/-(CLUBS)
      case "H"     => \/-(HEARTS)
      case "D"     => \/-(DIAMONDS)
      case unknown => -\/(s"Not a suit: $unknown")
    }
  }
}

case class Card(value: CardValue, suit: Suit)
case class Hand (cards: List[Card] = List.empty[Card])

sealed trait SuperPower { val order: Int }
case class  TwoPair      (value: CardValue) extends SuperPower { val order = 1 }
case class  ThreeOfAKind (value: CardValue) extends SuperPower { val order = 2 }
case class  FourOfAKind  (value: CardValue) extends SuperPower { val order = 3 }
case object NothingSoFar                    extends SuperPower { val order = 0 }

object SuperPower {
  val ordering: Ordering[SuperPower] = Ordering.by(e => e.order)
}






