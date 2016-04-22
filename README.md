# chinese-poker
A Haskell library to help users—both human and computer—play [Chinese Poker](https://en.wikipedia.org/wiki/Chinese_poker) against one another.

## Goals
The goal of this project is to create a set of datatypes and functions that represent the game of chinese poker. This core functionality will be used in two ways—to create a server that will allow multiple users (both human and machine) to play games of chinese poker against one another and to facilitate the creation of software that can intelligently play chinese poker on its own.

## Organization
The core logic of traditional poker hands is located in `src/Hand.hs`. This module exports four types—`Suit`, `Rank`, `Card`, and `Hand` defined as follows:
```haskell
data Suit = Diamonds | Clubs | Hearts | Spades deriving (Eq, Enum, Ord, Show)

data Rank = Two
          | Three
          -- etc...
          | King
          | Ace
          deriving (Eq, Enum, Ord, Show)

type Card = (Rank, Suit)

data Hand = Hand Card Card Card Card Card deriving (Eq, Show)
```
Hand is an instance of `Ord` whose implementation compares seperate `Hand`s according to the traditional rules of poker.

Hand also exposes a function called validateHands that will check to ensure that no cards are repeated in a given set of hands.
```haskell
validateHands :: [Hand] -> Bool
```

## Setup
To build the project, run `make setup` in the top-level directory.

## Tests
The tests are written using [Hspec](http://hspec.github.io/). To run the tests, simply type `make test`.


