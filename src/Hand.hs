module Hand (Suit(..), Rank(..), Card(..), Hand(Hand), validateHands) where

import Data.List (group, sort)
import Data.List.Unique (repeated)


data Suit = Diamonds
          | Clubs
          | Hearts
          | Spades deriving (Eq, Enum, Ord, Show)

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
          deriving (Eq, Enum, Ord, Show)

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
              deriving (Eq, Enum, Ord, Show)

type Card = (Rank, Suit)

data Hand = Hand Card Card Card Card Card deriving (Eq, Show)

getCards :: Hand -> [Card]
getCards (Hand a1 a2 a3 a4 a5) = [a1, a2, a3, a4, a5]

getSuits :: Hand -> [Suit]
getSuits = map snd . getCards

getSortedRanks :: Hand -> [Rank]
getSortedRanks = sort . map fst . getCards

validateHands :: [Hand] -> Bool
validateHands = (== 0) . length . repeated . concatMap getCards

instance Ord Hand where
  a `compare` b
    | handTypeA /= handTypeB = handTypeA `compare` handTypeB
    | otherwise              = (getSortedRanks a) `compare` (getSortedRanks b)
    where
      handTypeA = extractHandType a
      handTypeB = extractHandType b

extractHandType :: Hand -> HandType
extractHandType h = case increasingRankFrequencies of
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
    ranks = getSortedRanks h
    increasingRankFrequencies = sort $ map length $ group ranks
    isStraight = and $ zipWith (\a b -> b == succ a) ranks (drop 1 ranks)
    isWheelStraight = ranks == [Two, Three, Four, Five, Ace]
    isFlush = and $ map (== head (getSuits h)) (tail (getSuits h))
