# Rob's Poker Hands

This is incomplete.

I disappeared down the rabbit-hole of scalaz's State monad instead of solving the problem.

The State monad allows you to call a pure function which returns a value ```A``` and then record some state ```S```.

In my case, ```S``` is "the current list of cards in the player's hand" and ```A``` is the best "super power" in the player's hand (e.g. Two Pair, Three of a Kind, etc).

As of Tuesday 5th July, I only have "Two Pairs" being detected.  

