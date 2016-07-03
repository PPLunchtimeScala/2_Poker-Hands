package io.rob

import io.rob.algebra.{Card, CardValues, Hand, Suits}

import scalaz.State

/**
  * Created by rob on 03/07/16.
  */
object PokerAppS extends App {

  import CardValues._
  import Suits._

  val l1: List[Card] = List(Card(`2`, DIAMONDS), Card(`2`, HEARTS), Card(`4`, CLUBS), Card(`4`, SPADES))
  val l2: List[Card] = List(Card(`3`, DIAMONDS), Card(`3`, HEARTS), Card(`3`, CLUBS), Card(`4`, SPADES))
  val l3: List[Card] = List(Card(A, DIAMONDS), Card(`2`, HEARTS))

  type StateHand[Option[Card]] = State[Hand, Option[Card]]

  def addCard(card: Card) = {
    for {
      a <- State.init[Hand]
      _ <- State.modify { hand: Hand => Hand(card :: hand.cards) }
      c <- State.get
    } yield Some(card)
  }

  val emptyHand = for {
    _ <- State.init[Hand]
    _ <- State.modify((s: Hand) => s)
    r <- State.get
  } yield r.cards.lastOption


  val program = for {
    _  <- emptyHand
    _  <- addCard(Card(A, DIAMONDS))
    r0  <- State.get
    _  <- addCard(Card(J, CLUBS))
    r1 <- State.get
  } yield r0 :: r1 :: Nil


  println(program.eval(Hand(List.empty[Card])))
}
