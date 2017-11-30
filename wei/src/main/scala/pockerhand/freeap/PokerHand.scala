package pockerhand.freeap

import pockerhand.freeap.PokerHand.algebra.{Poker, PokerHand, PokerHandType, Rank, Suit}
import pockerhand.freeap.PokerHand.algebra.Suit.Suit
import pockerhand.freeap.PokerHand.interpreter.naturalTransToFuture

import scalaz._
import scalaz.{FreeAp, ValidationNel, ~>}
import scalaz.syntax.applicative._
import scala.language.higherKinds
import scalaz.concurrent.Future

object PokerHand {

  object typeclass {

    import algebra.PokerHandType._

    trait PokerHandEncoder[A] {
      def encode(value:A): PokerHandType.Value
    }

    implicit val intEncoder: PokerHandEncoder[Int] = new PokerHandEncoder[Int] {
      override def encode(value: Int): PokerHandType.Value = value match {
        case 1 => StraightFlush
        case 2 => FourOfAKind
        case 3 => FullHouse
        case 4 => Flush
        case 5 => Straight
        case 6 => ThreeOfAKind
        case 7 => TwoPair
        case 8 => OnePair
        case 9 => HighCard
        case _ => Invalid
      }
    }

//    implicit val booleanEncoder: PokerHandEncoder[Boolean] = new PokerHandEncoder[Boolean] {
//      override def encode(value: Boolean): PokerHandType = value match {
//
//      }
//    }
  }

  object algebra {

    object PokerHandType extends Enumeration {
      val StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPair, OnePair, HighCard, Invalid = Value
    }

    object Suit extends Enumeration {
      type Suit = Value
      val C, D, H, S = Value
    }

    sealed trait Rank

    implicit final class IntToRank(num:Int) {

      lazy val toRank: @@[Int, Rank] = Tag[Int, Rank](num)
    }

    implicit final class ShowRank(rank: @@[Int, Rank]) {

      lazy val show: String = Tag.unwrap(rank) match {

        case 1 => "A"
        case 11 => "J"
        case 12 => "Q"
        case 13 => "K"
        case a => a.toString
      }
    }
    
    case class Poker(rank: Int @@ Rank, suit: Suit)

    type PokerHand = List[Poker]

    sealed trait PokerHandValidation[A]

    case class IsStraight(hand: PokerHand) extends PokerHandValidation[Boolean]
    case class IsFlush(hand: PokerHand) extends PokerHandValidation[Boolean]
    case class Pairs(hand: PokerHand) extends PokerHandValidation[Int]
    case class XOfAKind(hand: PokerHand) extends PokerHandValidation[Int]
    case class HandSize(hand: PokerHand) extends PokerHandValidation[Int]
  }

  object interpreter {

    import pockerhand.freeap.PokerHand.algebra._

    type PokerHandCheck[A] = FreeAp[PokerHandValidation, A]

    def isStraight(hand: PokerHand) = FreeAp.lift[PokerHandValidation, Boolean](IsStraight(hand))
    def isFlush(hand: PokerHand) = FreeAp.lift[PokerHandValidation, Boolean](IsFlush(hand))
    def pairs(hand: PokerHand) = FreeAp.lift[PokerHandValidation, Int](Pairs(hand))
    def xOfAKind(hand: PokerHand) = FreeAp.lift[PokerHandValidation, Int](XOfAKind(hand))
    def handSize(hand: PokerHand) = FreeAp.lift[PokerHandValidation, Int](HandSize(hand))

    def program(hand: PokerHand):PokerHandCheck[PokerHandType.Value] = (
      handSize(hand) |@|
      isStraight(hand) |@|
      isFlush(hand) |@|
      pairs(hand) |@|
      xOfAKind(hand)
    )(toPokerHandType)

    def naturalTransToFuture: PokerHandValidation ~> Future =
      new (PokerHandValidation ~> Future) {

        def apply[A](fa: PokerHandValidation[A]) = fa match {

          case IsStraight(hand) => Future(checkStraight(hand))
          case IsFlush(hand) => Future(checkFlush(hand))
          case Pairs(hand) => Future(checkPairs(hand))
          case XOfAKind(hand) => Future(checkXOfAKind(hand))
          case HandSize(hand) => Future(hand.size)
        }
      }

    private lazy val toPokerHandType: (Int, Boolean, Boolean, Int, Int) => PokerHandType.Value = {

      // (handSize, isStraight, isFlush, pairCount, xOfAKind)

      case (5, false, false, 1, 3) => PokerHandType.FullHouse
      case (5, false, false, 1, _) => PokerHandType.OnePair
      case (5, false, false, 2, _) => PokerHandType.TwoPair
      case (5, _, _, _, 3)         => PokerHandType.ThreeOfAKind
      case (5, _, _, _, 4)         => PokerHandType.FourOfAKind
      case (5, true, false, _, _)  => PokerHandType.Straight
      case (5, false, true, _, _)  => PokerHandType.Flush
      case (5, true, true, _, _)   => PokerHandType.StraightFlush
      case (5, false, false, 0, 1) => PokerHandType.HighCard
      case _                       => PokerHandType.Invalid
    }

    private def checkStraight(hand:PokerHand): Boolean = {
      val ranks = hand map(_.rank)
      val start = Tag.unwrap(ranks.head)
      ranks.map(Tag.unwrap(_) - start).zipWithIndex.forall{
        case (i, r) => i == r
        case _      => false
      }
    }

    private def checkFlush(hand:PokerHand): Boolean = {
      val suits = hand map(_.suit)
      suits.forall(_ == suits.head)
    }

    private def checkXOfAKind(hand:PokerHand): Int = {

      groupByCount(hand).valuesIterator.max
    }

    private def checkPairs(hand:PokerHand): Int = {

      groupByCount(hand) count { case (_, count:Int) => count == 2 }
    }

    private def groupByCount(hand:PokerHand): Map[@@[Int, Rank], Int] = hand.groupBy(_.rank).mapValues(_.size)



//    assert(program(List("AC", "4D", "QC", "3H")) == "Invalid hand: Too few cards")
//    assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D")) == "Invalid hand: Too many cards")
//    assert(PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S")) == "Invalid hand: Three of Hearts appears two times")
//    assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")) == "High card: Queen of Clubs")
//
//    assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "10S")) == "One Pair: Fours")
//    assert(PokerApp.classifyHand(List("AC", "8D", "8C", "8H", "10S")) == "Three of a Kind: Eights")
//    assert(PokerApp.classifyHand(List("6C", "6D", "QC", "6H", "6S")) == "Four of a Kind: Sixes")
//    assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "QS")) == "Two Pair: Queens and Fours") //Larger card listed first
//    assert(PokerApp.classifyHand(List("5C", "9D", "5H", "9H", "9S")) == "Full House: Nines over Fives") //Larger card listed first
//
//    assert(PokerApp.classifyHand(List("AH", "7H", "QH", "4H", "10H")) == "Flush: Hearts")
//    assert(PokerApp.classifyHand(List("9C", "10D", "JC", "QH", "KS")) == "Straight: Nine to King")
//    assert(PokerApp.classifyHand(List("6D", "5D", "3D", "4D", "2D")) == "Straight Flush: Two to Six")
  }

}

object app extends App {

  import pockerhand.freeap.PokerHand.interpreter.program

  lazy val onePair = List(
    Poker(Tag[Int, Rank](1), Suit.C),
    Poker(Tag[Int, Rank](1), Suit.S),
    Poker(Tag[Int, Rank](2), Suit.D),
    Poker(Tag[Int, Rank](3), Suit.H),
    Poker(Tag[Int, Rank](4), Suit.C)
  )

  lazy val twoPairs = List(
    Poker(Tag[Int, Rank](1), Suit.C),
    Poker(Tag[Int, Rank](1), Suit.S),
    Poker(Tag[Int, Rank](2), Suit.D),
    Poker(Tag[Int, Rank](2), Suit.H),
    Poker(Tag[Int, Rank](3), Suit.C)
  )

  lazy val threeOfAKind = List(
    Poker(Tag[Int, Rank](1), Suit.C),
    Poker(Tag[Int, Rank](1), Suit.S),
    Poker(Tag[Int, Rank](1), Suit.D),
    Poker(Tag[Int, Rank](2), Suit.H),
    Poker(Tag[Int, Rank](3), Suit.C)
  )

  lazy val fourOfAKind = List(
    Poker(Tag[Int, Rank](1), Suit.C),
    Poker(Tag[Int, Rank](1), Suit.S),
    Poker(Tag[Int, Rank](1), Suit.D),
    Poker(Tag[Int, Rank](1), Suit.H),
    Poker(Tag[Int, Rank](3), Suit.C)
  )

  lazy val fullHouse = List(
    Poker(Tag[Int, Rank](1), Suit.C),
    Poker(Tag[Int, Rank](1), Suit.S),
    Poker(Tag[Int, Rank](2), Suit.D),
    Poker(Tag[Int, Rank](2), Suit.H),
    Poker(Tag[Int, Rank](2), Suit.C)
  )

  lazy val flush = List(
    Poker(Tag[Int, Rank](1), Suit.C),
    Poker(Tag[Int, Rank](2), Suit.C),
    Poker(Tag[Int, Rank](5), Suit.C),
    Poker(Tag[Int, Rank](6), Suit.C),
    Poker(Tag[Int, Rank](7), Suit.C)
  )

  lazy val straight = List(
    Poker(Tag[Int, Rank](1), Suit.C),
    Poker(Tag[Int, Rank](2), Suit.S),
    Poker(Tag[Int, Rank](3), Suit.D),
    Poker(Tag[Int, Rank](4), Suit.H),
    Poker(Tag[Int, Rank](5), Suit.C)
  )

  lazy val straightFlush = List(
    Poker(Tag[Int, Rank](1), Suit.C),
    Poker(Tag[Int, Rank](2), Suit.C),
    Poker(Tag[Int, Rank](3), Suit.C),
    Poker(Tag[Int, Rank](4), Suit.C),
    Poker(Tag[Int, Rank](5), Suit.C)
  )

  lazy val highCard = List(
    Poker(Tag[Int, Rank](13), Suit.S),
    Poker(Tag[Int, Rank](2), Suit.D),
    Poker(Tag[Int, Rank](8), Suit.C),
    Poker(Tag[Int, Rank](7), Suit.H),
    Poker(Tag[Int, Rank](4), Suit.C)
  )

  val testCases = List(
    (onePair, PokerHandType.OnePair),
    (twoPairs, PokerHandType.TwoPair),
    (threeOfAKind, PokerHandType.ThreeOfAKind),
    (fourOfAKind, PokerHandType.FourOfAKind),
    (fullHouse, PokerHandType.FullHouse),
    (flush, PokerHandType.Flush),
    (straight, PokerHandType.Straight),
    (straightFlush, PokerHandType.StraightFlush),
    (highCard, PokerHandType.HighCard)
  )

  testCases.foreach{case (hand, expected) =>
    val actual = program(hand).foldMap(naturalTransToFuture).run
    println(s"$expected - $actual")
    assert(expected == actual)
  }

}
