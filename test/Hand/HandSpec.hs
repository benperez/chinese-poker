module Hand.HandSpec (main, spec) where

import Data.List (sort)

import Test.Hspec

import Hand (Suit(..), Rank(..), Card(..), Hand(Hand))

highCard      = Hand Card {rank = Ace,   suit = Diamonds}
                     Card {rank = Seven, suit = Hearts}
                     Card {rank = Five,  suit = Clubs}
                     Card {rank = Three, suit = Diamonds}
                     Card {rank = Two,   suit = Spades}
pair          = Hand Card {rank = Ace,   suit = Clubs}
                     Card {rank = Ace,   suit = Diamonds}
                     Card {rank = Nine,  suit = Hearts}
                     Card {rank = Six,   suit = Spades}
                     Card {rank = Four,  suit = Diamonds}
twoPair       = Hand Card {rank = King,  suit = Hearts}
                     Card {rank = King,  suit = Spades}
                     Card {rank = Jack,  suit = Clubs}
                     Card {rank = Jack,  suit = Diamonds}
                     Card {rank = Nine,  suit = Diamonds}
trips         = Hand Card {rank = Queen, suit = Spades}
                     Card {rank = Queen, suit = Hearts}
                     Card {rank = Queen, suit = Diamonds}
                     Card {rank = Five,  suit = Spades}
                     Card {rank = Nine,  suit = Clubs}
straight      = Hand Card {rank = Queen, suit = Spades}
                     Card {rank = Jack,  suit = Diamonds}
                     Card {rank = Ten,   suit = Clubs}
                     Card {rank = Nine,  suit = Spades}
                     Card {rank = Eight, suit = Hearts}
flush         = Hand Card {rank = King,  suit = Spades}
                     Card {rank = Jack,  suit = Spades}
                     Card {rank = Nine,  suit = Spades}
                     Card {rank = Seven, suit = Spades}
                     Card {rank = Three, suit = Spades}
fullHouse     = Hand Card {rank = King,  suit = Hearts}
                     Card {rank = King,  suit = Diamonds}
                     Card {rank = King,  suit = Spades}
                     Card {rank = Five,  suit = Hearts}
                     Card {rank = Five,  suit = Clubs}
fourOfAKind   = Hand Card {rank = Five,  suit = Diamonds}
                     Card {rank = Five,  suit = Spades}
                     Card {rank = Five,  suit = Hearts}
                     Card {rank = Five,  suit = Clubs}
                     Card {rank = Three, suit = Hearts}
straightFlush = Hand Card {rank = Eight, suit = Clubs}
                     Card {rank = Seven, suit = Clubs}
                     Card {rank = Six,   suit = Clubs}
                     Card {rank = Five,  suit = Clubs}
                     Card {rank = Four,  suit = Clubs}
unsortedHandArray = [ trips
                    , fullHouse
                    , highCard
                    , fourOfAKind
                    , pair
                    , twoPair
                    , straightFlush
                    , straight
                    , flush
                    ]

spec :: Spec
spec = do
  describe "absolute" $ do
    it "correctly sorts a deck of hands" $
      sort unsortedHandArray `shouldBe` [ highCard
                                        , pair
                                        , twoPair
                                        , trips
                                        , straight
                                        , flush
                                        , fullHouse
                                        , fourOfAKind
                                        , straightFlush
                                        ]

main :: IO ()
main = hspec spec
