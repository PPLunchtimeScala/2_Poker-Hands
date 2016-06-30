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
        case e:_ => Failure(e)
      }
    }
  }

  object Suits extends Enumeration {
    type Suit = Value
    val CLUBS, SPADES, DIAMONDS, HEARTS = Value

    def fromString(s: String) = {
      s.toUpperCase match {
        case "S" => SPADES
        case "C" => CLUBS
        case "H" => HEARTS
        case "D" => DIAMONDS
      }
    }
  }

  case class Card(value: CardValue, suit: Suit)

  def toCard(s: String): Try[Card] = {
    for {
      value <- CardValues.withName(s.dropRight(1))
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
  //CardValues.withName()

}
