#PP Lunchtime Scala 
##Puzzle Number 2 - Poker-Hands Part 1
###Due 05/07/2016

##Description
Determine the status of a poker hand.

A poker hand is given as a list of 5 cards. 
Each card is a simple String that is a combination of the suit and the value of the card. eg 5C is the 5 of clubs.
The four suits are - 
D: Diamonds
H: Hearts
C: Clubs
S: Spades

The 13 values are 
A: ace
2,, 3, 4...10
J: Jack
Q: Queen
K: King

You are free to model cards and hands any way you like but the code you write should be able to take a hand like such:
```scala
val player1 = List("AC", "4D", "QC", "3H", "10S")
```

Your code should take a hand and identify it based on the standard rules of poker.
The possible hands are:

##Tests
If you code is correct, the following asserts should pass:


