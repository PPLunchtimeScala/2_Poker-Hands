#PP Lunchtime Scala 
##Puzzle Number 2 - Poker-Hands Part 1
###Due 05/07/2016

##Submissions
* Submit your solution in a single scala worksheet
* Push the worksheet to the repo in a new folder with your name
* Use the asserts listed below to test your code
* No throwing exceptions
* No loops

Note:
* Come to the meetup whether you have done all, part or none of the problem.
* Break any of the rules you want. The only goal is that you learn something.

##Description
Determine the status of a poker hand.

A poker hand is given as a list of 5 cards. 
Each card is given as a combination of the suit and the value of the card. eg 5C is the 5 of clubs.
The four suits are:
* D: Diamonds
* H: Hearts
* C: Clubs
* S: Spades

The 13 values are:
* A: ace
* 2, 3, 4...10
* J: Jack
* Q: Queen
* K: King

You are free to model cards and hands any way you like, but your code should be able to take a hand defined as a list of strings as in the following example.
```scala
object PokerApp {
  def classifyHand(hand: List[String]): String = {
    ???
  }
}
val hand1 = List("AC", "4D", "QC", "3H", "10S")
PokerApp.classifyHand(hand1)
```

Your code should be able to classify hands based on the standard rules of poker. 

The possible hands are:
* Straight flush
* Four of a kind
* Full house
* Flush
* Straight
* Three of a kind
* Two pair
* One pair
* High card

These are listed and explained at https://en.wikipedia.org/wiki/List_of_poker_hands

##Tests
If your code is correct, the following asserts should pass. 
Copy and paste them into the bottom of your worksheet and use them as the acceptance criteria for your code.

```scala
//Basic functionality
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H")) == "Invalid hand: Too few cards")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D")) == "Invalid hand: Too many cards")
assert(PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S")) == "Invalid hand: Three of Hearts appears two times")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")) == "High card: Queen of Clubs")
//Intermediate functionality
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "10S")) == "One Pair: Fours")
assert(PokerApp.classifyHand(List("AC", "8D", "8C", "8H", "10S")) == "Three of a Kind: Eights")
assert(PokerApp.classifyHand(List("6C", "6D", "QC", "6H", "6S")) == "Four of a Kind: Sixes")
assert(PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "QS")) == "Two Pair: Fours and Queens")
assert(PokerApp.classifyHand(List("5C", "9D", "5H", "9H", "9S")) == "Full House: Nines over Fives")
//Advanced functionality
assert(PokerApp.classifyHand(List("AH", "7H", "QH", "4H", "10H")) == "Flush: Hearts")
assert(PokerApp.classifyHand(List("9C", "10D", "JC", "QH", "KS")) == "Straight: Nine to King")
assert(PokerApp.classifyHand(List("10D", "JD", "QD", "KD", "AD")) == "Straight Flush: Diamonds")
```
