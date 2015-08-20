# chinese-poker
A Haskell library to help users—both human and computer—play [Chinese Poker](https://en.wikipedia.org/wiki/Chinese_poker) against one another.

## Goals
The goal of this project is to create a set of datatypes and functions that represent the game of chinese poker. This core functionality will be used in two ways—to create a server that will allow multiple users (both human and machine) to play games of chinese poker against one another and to facilitate the creation of software that can intelligently play chinese poker on its own.

## Organization
The core logic of traditional poker hands is located in `src/Hand.hs`. This module exports four types—`Suit`, `Rank`, `Card`, and `Hand` defined as follows:
```haskell
data Suit = Diamonds | Clubs | Hearts | Spades deriving (Eq, Show)

data Rank = Two
          | Three
          -- etc...
          | King
          | Ace
          deriving (Eq, Ord, Enum, Show)

data Card = Card {rank :: Rank, suit :: Suit} deriving (Eq, Show)

data Hand = Hand Card Card Card Card Card deriving (Eq, Show)
```
`Card` and `Hand` are both instances of `Ord`. The implementation of `Ord` that `Hand` provides compares seperate `Hand`s according to the traditional rules of poker

## Setup
To build the project, run `make setup` in the top-level directory. This will output an executable file named `chinese-poker` (built from `src/Main.hs`) into the `.cabal-sandbox/bin/` directory.


