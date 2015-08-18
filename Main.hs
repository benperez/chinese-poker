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

data Card = Card Rank Suit deriving (Eq, Show)

instance Ord Card where
  (Card rank1 _) `compare` (Card rank2 _) = rank1 `compare` rank2

data Hand = Hand Card Card Card Card Card deriving (Eq, Show)

--TODO - this is where hand comparison logic goes
instance Ord Hand where
  (Hand a _ _ _ _) `compare` (Hand b _ _ _ _) = a `compare` b

extractFrequencyHandTypes :: Hand -> HandType
extractFrequencyHandTypes (Hand c1 c2 c3 c4 c5) = case rankFreqs of
  [4, 1]       -> Quads
  [3, 2]       -> FullHouse
  [3, 1, 1]    -> Trips
  [2, 2, 1]    -> TwoPair
  [2, 1, 1, 1] -> Pair
  _            -> HighCard
  where
    ranksEqual (Card rank1 _) (Card rank2 _) = rank1 == rank2
    rankFreqs = map length . groupBy ranksEqual . sort $ [c1, c2, c3, c4, c5]
