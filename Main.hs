
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
