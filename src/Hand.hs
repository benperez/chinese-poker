module Hand (Suit(..), Rank(..), Card(..), Hand(Hand)) where

import Data.List (group, sort)


data Suit = Diamonds
          | Clubs
          | Hearts
          | Spades deriving (Eq, Show)

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Eq, Ord, Enum, Show)

data HandType = HighCard
              | Pair
              | TwoPair
              | Trips
              --a wheel straight is a 5 high straight
              | WheelStraight
              | Straight
              | Flush
              | FullHouse
              | Quads
              --a wheel straight flush is a 5 high straight flush
              | WheelStraightFlush
              | StraightFlush
              deriving (Eq, Ord, Enum, Show)

data Card = Card { rank :: Rank
                 , suit :: Suit
                 } deriving (Eq, Show)

instance Ord Card where
  card1 `compare` card2 = (rank card1) `compare` (rank card2)

data Hand = Hand Card Card Card Card Card deriving (Eq, Show)

sortedCards :: Hand -> [Card]
sortedCards (Hand a1 a2 a3 a4 a5) = sort $ [a1, a2, a3, a4, a5]

instance Ord Hand where
  a `compare` b
    | handTypeA /= handTypeB = handTypeA `compare` handTypeB
    | otherwise              = (sortedCards a) `compare` (sortedCards b)
    where
      handTypeA = extractHandType a
      handTypeB = extractHandType b

extractHandType :: Hand -> HandType
extractHandType h = case rankFrequencies of
  [1, 4]          -> Quads
  [2, 3]          -> FullHouse
  [1, 1, 3]       -> Trips
  [1, 2, 2]       -> TwoPair
  [1, 1, 1, 2]    -> Pair
  [1, 1, 1, 1, 1] -> case (isStraight, isWheelStraight, isFlush) of
    (True,  False, True)  -> StraightFlush
    (False, True,  True)  -> WheelStraightFlush
    (False, False, True)  -> Flush
    (True,  False, False) -> Straight
    (False, True,  False) -> WheelStraight
    (False, False, False) -> HighCard
  where
    ranks = map rank $ sortedCards h
    rankFrequencies = sort $ map length $ group ranks
    isStraight = and $ zipWith (\a b -> b == succ a) ranks (drop 1 ranks)
    isWheelStraight = ranks == [Two, Three, Four, Five, Ace]
    suits = map suit $ sortedCards h
    isFlush = and $ map (== head suits) (tail suits)
