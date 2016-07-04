package io.rob

import io.rob.model._


/**
  * Created by rob on 03/07/16.
  */
object PokerAppS extends App {

  import CardValues._
  import Suits._
  import scalaz.State

  type SuperPowers = Set[SuperPower]
  type SuperPowerTest = List[Card] => Boolean
  type StateHand[SuperPower] = State[Hand, SuperPower]

  val l1: List[Card] = List(Card(`2`, DIAMONDS), Card(`2`, HEARTS), Card(`4`, CLUBS), Card(`4`, SPADES))
  val l2: List[Card] = List(Card(`3`, DIAMONDS), Card(`3`, HEARTS), Card(`3`, CLUBS), Card(`4`, SPADES))
  val l3: List[Card] = List(Card(A, DIAMONDS), Card(`2`, HEARTS))

  val containsSameCardValues: Int => List[Card] => Boolean = {
    targetNumberOfCards => cards => cards.size == targetNumberOfCards
  }

  val twoPairTest: List[Card] => Option[SuperPower] = cards => {
    val groups: Map[CardValue, List[Card]] = cards.groupBy(_.value)

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
  )

  def contains(cards: Map[CardValue, List[Card]])(f: List[Card] => Boolean) = {
    cards.map { case (k, v) => f(v) }.exists(_ == true)
  }

  def addCard(card: Card) = {
    for {
      a <- State.init[Hand]
      _ <- State.modify { h: Hand => {
          val cards = card :: h.cards
          Hand(cards, identifySuperPowers(cards))
        }
      }
      c <- State.get
    } yield Some(card)
  }

  def emptyHand() = for {
    _    <- State.init[Hand]
    _    <- State.modify((h: Hand) => Hand())
    hand <- State.get
  } yield NothingSoFar


  def identifySuperPowers(cards: List[Card]): SuperPowers = {
    superPowerTests.foldLeft(Set.empty[SuperPower]) {(z, b) =>
      b(cards) match {
        case None => z
        case Some(power) => z + power
      }
    }
  }

  //  def checkForBestSuperPower(): StateHand[SuperPower] =  for {
  //    _ <- State.init[Hand]
  //    hand <= State.get
  //  }

  val program = for {
    _   <- emptyHand()
    _   <- addCard(Card(`3`, DIAMONDS))
    _   <- addCard(Card(`3`, CLUBS))
    _   <- addCard(Card(`3`, SPADES))
    r1  <- State.get
  } yield r1

  println(program.eval(Hand()).superPowers)
}
