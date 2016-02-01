import Hand (Suit(..), Rank(..), Card(..), Hand(Hand))

-- | 'main' runs the main program
main :: IO ()
main = print $ straightFlush `compare` quads
  where
    straightFlush = Hand Card {rank = Three, suit = Spades}
                         Card {rank = Four,  suit = Spades}
                         Card {rank = Five,  suit = Spades}
                         Card {rank = Six,   suit = Spades}
                         Card {rank = Seven, suit = Spades}
    quads = Hand Card {rank = Eight, suit = Spades}
                 Card {rank = Eight, suit = Hearts}
                 Card {rank = Eight, suit = Diamonds}
                 Card {rank = Eight, suit = Clubs}
                 Card {rank = Seven, suit = Clubs}
