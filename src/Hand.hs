module Hand (Suit(..), Rank(..), Card(..), Hand(Hand), validateHands) where

import Data.List (group, sort, sortOn)


data Suit = Diamonds | Clubs | Hearts | Spades
  deriving (Bounded, Eq, Enum, Ord, Show)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Bounded, Eq, Enum, Ord, Show)

type Card = (Rank, Suit)

data Hand = Hand Card Card Card Card Card
  deriving (Show)

data HandType = HighCard
              | Pair
              | TwoPair
              | Trips
              | WheelStraight
              | Straight
              | Flush
              | FullHouse
              | Quads
              | WheelStraightFlush
              | StraightFlush
              deriving (Eq, Ord, Show)

getCards :: Hand -> [Card]
getCards (Hand a1 a2 a3 a4 a5) = [a1, a2, a3, a4, a5]

getCardsSortedByRank :: Hand -> [Card]
getCardsSortedByRank = sortOn fst . getCards

repeated :: Ord a => [a] -> [a]
repeated = map head . filter ((>1) . length) . group . sort

validateHands :: [Hand] -> Bool
validateHands = null . repeated . concatMap getCards

isStraight :: [Rank] -> Bool
isStraight ranks = and $ zipWith (\a b -> b == succ a) ranks (drop 1 ranks)

isWheelStraight :: [Rank] -> Bool
isWheelStraight = (== [Two, Three, Four, Five, Ace])

isFlush :: [Suit] -> Bool
isFlush suits = all (== head suits) (tail suits)

extractHandType :: Hand -> (HandType, (Rank, Rank, Rank, Rank, Rank))
extractHandType h = case increasingRankFrequencies of
  [(1,[a]),   (4,[b,c,d,e])]                        -> (Quads,     (e, d, c, b, a))
  [(2,[a,b]), (3,[c,d,e])]                          -> (FullHouse, (e, d, c, b, a))
  [(1,[a]),   (1,[b]),   (3,[c,d,e])]               -> (Trips,     (e, d, c, b, a))
  [(1,[a]),   (2,[b,c]), (2,[d,e])]                 -> (TwoPair,   (e, d, c, b, a))
  [(1,[a]),   (1,[b]),   (1,[c]), (2,[d,e])]        -> (Pair,      (e, d, c, b, a))
  [(1,[a]),   (1,[b]),   (1,[c]), (1,[d]), (1,[e])] -> case (isStraight rs, isWheelStraight rs, isFlush ss) of
    (True,  False, True)  -> (StraightFlush,      (e, d, c, b, a))
    (False, True,  True)  -> (WheelStraightFlush, (e, d, c, b, a))
    (False, False, True)  -> (Flush,              (e, d, c, b, a))
    (True,  False, False) -> (Straight,           (e, d, c, b, a))
    (False, True,  False) -> (WheelStraight,      (e, d, c, b, a))
    (False, False, False) -> (HighCard,           (e, d, c, b, a))
  where
    sortedCards = getCardsSortedByRank h
    rs = map fst sortedCards
    ss = map snd sortedCards
    ranksEqual a b = fst a == fst b
    increasingRankFrequencies = sortOn fst .
                                map (\g -> (length g, g)) .
                                group $ rs

instance Ord Hand where
  a `compare` b
    | handTypeA /= handTypeB = handTypeA `compare` handTypeB
    | otherwise              = ranksA    `compare` ranksB
    where
      (handTypeA, ranksA) = extractHandType a
      (handTypeB, ranksB) = extractHandType b

instance Eq Hand where
  a == b
    | handTypeA /= handTypeB = False
    | otherwise              = ranksA == ranksB
    where
      (handTypeA, ranksA) = extractHandType a
      (handTypeB, ranksB) = extractHandType b
