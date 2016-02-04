module Hand.HandSpec (main, spec) where

import Data.List (group, sort)

import Test.Hspec

import Hand (Suit(..), Rank(..), Card(..), Hand(Hand), validateHands)

highCard      = Hand (Ace,   Diamonds)
                     (Seven, Hearts)
                     (Five,  Clubs)
                     (Three, Diamonds)
                     (Two,   Spades)
pair          = Hand (Ace,   Clubs)
                     (Ace,   Diamonds)
                     (Nine,  Hearts)
                     (Six,   Spades)
                     (Four,  Diamonds)
twoPair       = Hand (King,  Hearts)
                     (King,  Spades)
                     (Jack,  Clubs)
                     (Jack,  Diamonds)
                     (Nine,  Diamonds)
trips         = Hand (Queen, Spades)
                     (Queen, Hearts)
                     (Queen, Diamonds)
                     (Five,  Spades)
                     (Nine,  Clubs)
straight      = Hand (Queen, Spades)
                     (Jack,  Diamonds)
                     (Ten,   Clubs)
                     (Nine,  Spades)
                     (Eight, Hearts)
flush         = Hand (King,  Spades)
                     (Jack,  Spades)
                     (Nine,  Spades)
                     (Seven, Spades)
                     (Three, Spades)
fullHouse     = Hand (King,  Hearts)
                     (King,  Diamonds)
                     (King,  Spades)
                     (Five,  Hearts)
                     (Five,  Clubs)
fourOfAKind   = Hand (Five,  Diamonds)
                     (Five,  Spades)
                     (Five,  Hearts)
                     (Five,  Clubs)
                     (Three, Hearts)
straightFlush = Hand (Eight, Clubs)
                     (Seven, Clubs)
                     (Six,   Clubs)
                     (Five,  Clubs)
                     (Four,  Clubs)
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
invalidHand   = Hand (Eight, Clubs)
                     (Eight, Clubs)
                     (Three, Hearts)
                     (Five,  Spades)
                     (Four,  Diamonds)

spec :: Spec
spec = do
  describe "hand implementation of Ord" $ do
    it "sorts a deck of hands" $
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
    it "breaks ties when the hands have the same type" $
      let hc1    = Hand (Ace, Diamonds) (Seven, Hearts) (Five, Clubs) (Three, Diamonds) (Two, Spades)
          hc2    = Hand (King, Diamonds) (Seven, Hearts) (Five, Clubs) (Three, Diamonds) (Two, Spades)
          two1   = Hand (King, Hearts) (King, Spades) (Jack, Clubs) (Jack, Diamonds) (Nine, Diamonds)
          two2   = Hand (King, Hearts) (King, Spades) (Ten, Clubs) (Ten, Diamonds) (Nine, Diamonds)
          f1     = Hand (King, Spades) (Jack, Spades) (Nine, Spades) (Seven, Spades) (Three, Spades)
          f2     = Hand (King, Hearts) (Jack, Hearts) (Nine, Hearts) (Seven, Hearts) (Three, Hearts)
      in group (sort [hc1, hc2, two1, two2, f1, f2]) `shouldBe` [ [hc2]
                                                                , [hc1]
                                                                , [two2]
                                                                , [two1]
                                                                , [f1, f2]
                                                                ]
  describe "hand validation" $ do
    it "validates a correct hand" $
      validateHands [flush] `shouldBe` True

    it "invalidates a hand with a repeated card" $
      validateHands [invalidHand] `shouldBe` False

    it "validates two hands with different cards" $
      validateHands [highCard, twoPair] `shouldBe` True

    it "invalidates two hands with the same card" $
      validateHands [highCard, pair] `shouldBe` False

main :: IO ()
main = hspec spec
