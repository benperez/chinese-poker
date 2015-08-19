import Data.List (groupBy, sort)

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
    | handTypeA /= handTypeB        = handTypeA `compare` handTypeB
    | otherwise                     = sortedHandlistA `compare` sortedHandlistB
    where
      handTypeA = case extractFiveCardHandType a of
        (Just fiveCardHandTypeA) -> max fiveCardHandTypeA (extractFreqHandType a)
        _                        -> extractFreqHandType a
      handTypeB = case extractFiveCardHandType b of
        (Just fiveCardHandTypeB) -> max fiveCardHandTypeB (extractFreqHandType b)
        _                        -> extractFreqHandType b
      sortedHandlistA = sortedCards a
      sortedHandlistB = sortedCards b

extractFreqHandType :: Hand -> HandType
extractFreqHandType h = case rankFreqs of
  [4, 1]       -> Quads
  [3, 2]       -> FullHouse
  [3, 1, 1]    -> Trips
  [2, 2, 1]    -> TwoPair
  [2, 1, 1, 1] -> Pair
  _            -> HighCard
  where
    ranksEqual card1 card2 = (rank card1) == (rank card2)
    rankFreqs = map length . groupBy ranksEqual . sortedCards $ h

extractFiveCardHandType :: Hand -> Maybe HandType
extractFiveCardHandType h = case (isStraight, isWheelStraight, isFlush) of
  (True,  False, True)  -> Just StraightFlush
  (False, True,  True)  -> Just WheelStraightFlush
  (False, False, True)  -> Just Flush
  (True,  False, False) -> Just Straight
  (False, True,  False) -> Just WheelStraight
  (False, False, False) -> Nothing
  where
    ranks = map rank $ sortedCards h
    suits = map suit $ sortedCards h
    isStraight = and $ zipWith (\a b -> b == succ a) ranks (drop 1 ranks)
    isWheelStraight = ranks == [Two, Three, Four, Five, Ace]
    isFlush = and $ map (== head suits) (tail suits)
