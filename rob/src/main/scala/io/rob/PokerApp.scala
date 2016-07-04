package io.rob

import io.rob.model.{Card, CardValues, Hand, Suits}

import scala.util.Try

object PokerApp extends App {
  import CardValues._
  import Suits._

  val containsSameCardValues: Int => List[Card] => Boolean =
    targetNumberOfCards => cards => cards.size == targetNumberOfCards

  val twoPair = containsSameCardValues(2)
  val threeOfAKind = containsSameCardValues(3)
  val fourOfAKind = containsSameCardValues(4)

  def contains(cards: Map[CardValue, List[Card]])(f: List[Card] => Boolean): Boolean = {
    cards.map { case (k, v) => f(v) }.exists(_ == true)
  }

  def highCard(cards: List[Card]): Option[Card] = {
    cards.foldLeft(Option.empty[Card])((z, b) => {
      z match {
        case None => Some(b)
        case Some(card) => if (card.value > b.value) Some(card) else Some(b)
      }
    })
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
