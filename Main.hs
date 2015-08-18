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
              | Straight
              | Flush
              | FullHouse
              | Quads
              | StraightFlush
              deriving (Eq, Ord, Enum, Show)

data Card = Card { rank :: Rank
                 , suit :: Suit
                 } deriving (Eq, Show)

instance Ord Card where
  card1 `compare` card2 = (rank card1) `compare` (rank card2)

data Hand = Hand Card Card Card Card Card deriving (Eq, Show)

instance Ord Hand where
  a@(Hand a1 a2 a3 a4 a5) `compare` b@(Hand b1 b2 b3 b4 b5)
    | handTypeA /= handTypeB = handTypeA `compare` handTypeB
    | otherwise              = sortedHandlistA `compare` sortedHandlistB
    where
      handTypeA = case extractFiveCardHandType a of
        (Just fiveCardHandTypeA) -> max fiveCardHandTypeA (extractFreqHandType a)
        _                        -> extractFreqHandType a
      handTypeB = case extractFiveCardHandType b of
        (Just fiveCardHandTypeB) -> max fiveCardHandTypeB (extractFreqHandType b)
        _                        -> extractFreqHandType b
      sortedHandlistA = sort [a1, a2, a3, a4, a5]
      sortedHandlistB = sort [b1, b2, b3, b4, b5]

extractFreqHandType :: Hand -> HandType
extractFreqHandType (Hand c1 c2 c3 c4 c5) = case rankFreqs of
  [4, 1]       -> Quads
  [3, 2]       -> FullHouse
  [3, 1, 1]    -> Trips
  [2, 2, 1]    -> TwoPair
  [2, 1, 1, 1] -> Pair
  _            -> HighCard
  where
    ranksEqual card1 card2 = (rank card1) == (rank card2)
    rankFreqs = map length . groupBy ranksEqual . sort $ [c1, c2, c3, c4, c5]

extractFiveCardHandType :: Hand -> Maybe HandType
extractFiveCardHandType (Hand c1 c2 c3 c4 c5) = case (isStraight, isFlush) of
  (True, True)   -> Just StraightFlush
  (False, True)  -> Just Flush
  (True, False)  -> Just Straight
  (False, False) -> Nothing
  where
    cardList = [c1, c2, c3, c4, c5]
    rankList = map rank cardList
    suitList = map suit cardList
    isStraight = and $ zipWith (\a b -> b == succ a) rankList (drop 1 rankList)
    isFlush = and $ map (== head suitList) (tail suitList)
