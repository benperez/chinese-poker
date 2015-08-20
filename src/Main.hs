import Hand (Suit(..), Rank(..), Card(..), Hand(Hand))

-- | 'main' runs the main program
main :: IO ()
main = do
  putStrLn $ show $ straightFlush `compare` quads
  where
    straightFlush = Hand Card {rank = Three, suit = Spades}
                         Card {rank = Four,  suit = Spades}
                         Card {rank = Five,  suit = Spades}
                         Card {rank = Six,   suit = Spades}
                         Card {rank = Seven, suit = Spades}
    quads = Hand Card {rank = Three, suit = Spades}
                 Card {rank = Three, suit = Hearts}
                 Card {rank = Three, suit = Diamonds}
                 Card {rank = Three, suit = Clubs}
                 Card {rank = Seven, suit = Spades}
