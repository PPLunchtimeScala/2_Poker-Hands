#PP Lunchtime Scala 
##Puzzle Number 2 - Poker-Hands Part 1
###Due 05/07/2016

##Submissions
* Submit your solution in a single scala worksheet
* Push the worksheet to the repo in a new folder with your name
* All the asserts listed below should pass
* No throwing exceptions
* No loops

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

You are free to model cards and hands any way you like, but your code should be able to take a hand defined as a list of strings.

An sample hand is:
```scala
val player1 = List("AC", "4D", "QC", "3H", "10S")
```

Your code should take a hand and identify it based on the standard rules of poker.

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

Use these as the acceptance criteria for your code.
```scala
assert(PokerApp.identifyHand(List("AC", "4D", "QC", "3H") == "Invalid hand: Too few cards")
assert(PokerApp.identifyHand(List("AC", "4D", "QC", "3H", "10S") == "Invalid hand: Too many cards")
assert(PokerApp.identifyHand(List("AC", "4D", "QC", "3H", "10S") == "High card: Queen of Clubs")
```
