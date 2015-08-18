import Data.List (groupBy, sort)

data Suit = Diamonds
          | Clubs
          | Hearts
          | Spades deriving (Eq, Enum, Show)

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

--TODO - this is where hand comparison logic goes
instance Ord Hand where
  (Hand a _ _ _ _) `compare` (Hand b _ _ _ _) = a `compare` b

extractFrequencyHandType :: Hand -> HandType
extractFrequencyHandType (Hand c1 c2 c3 c4 c5) = case rankFreqs of
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
