package io.rob

import io.rob.model._

import scalaz.\/


object PokerAppS extends App {

  import CardValues._
  import scalaz.State

  type SuperPowers = Set[SuperPower]
  type SuperPowerTest = List[Card] => Boolean
  type StateHand[SuperPower] = State[Hand, SuperPower]

  implicit val ordering = SuperPower.ordering

  def toCard(s: String): String \/ Card = {
    for {
      value <- CardValues.fromString(s.dropRight(1))
      suit  <- Suits.fromString(s.takeRight(1))
    } yield Card(value, suit)
  }

  // This "twoPairTest" is rubbish code.  It started life as something else and was then abandoned!
  val twoPairTest: Hand => Option[SuperPower] = hand => {
    val groups: Map[CardValue, List[Card]] = hand.cards.groupBy(_.value)

    val containsSameCardValues: Int => List[Card] => Boolean = {
      targetNumberOfCards => cards => cards.size >= targetNumberOfCards
    }

    val twos = groups.find {
      case (cardValue, listOfCards) => containsSameCardValues(2)(listOfCards)
    }

    twos match {
      case None         => None
      case Some((k, v)) => Some(TwoPair(k))
      case _            => None
    }
  }

  val superPowerTests = List(
    twoPairTest
//    threeOfAKindTest,
//    fourOfAKindTest
//    fullHouse
//    etc
  )

  def identifySuperPowers(hand: Hand): SuperPowers = {
    superPowerTests.foldLeft(Set[SuperPower](NothingSoFar)) {(z, test) =>
      test(hand) match {
        case None => z
        case Some(power) => z + power
      }
    }
  }


  def emptyHand() = for {
    _  <- State.init[Hand]
    _  <- State.modify((h: Hand) => Hand())
    _  <- State.get
  } yield NothingSoFar


  def addCard(s: String): StateHand[SuperPower] = {
    // FIXME: How do I use a Try with a State?
    val card = toCard(s).toOption.get

    for {
      _    <- State.init[Hand]
      _    <- State.modify { h: Hand => Hand(card :: h.cards) }
      hand <- State.get
    } yield identifySuperPowers(hand).max
  }

  val program = for {
    _    <- emptyHand()
    _    <- addCard("3D")
    _    <- addCard("3C")
    _    <- addCard("3S")
    _    <- addCard("JS")
    sp   <- addCard("JD")
    hand <- State.get
  } yield s"Best superPower in $hand is $sp"

  println(program.eval(Hand()))
}
