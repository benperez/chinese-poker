module Hand.HandSpec (main, spec) where

import Data.List (sort)

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
  describe "hand validation" $ do
    it "correctly identifies a valid hand" $
      validateHands [flush] `shouldBe` True

    it "correctly identifies an invalid hand" $
      validateHands [invalidHand] `shouldBe` False

    it "correctly identifies two valid hands" $
      validateHands [highCard, twoPair] `shouldBe` True

    it "correctly identifies two invalid hands" $
      validateHands [highCard, pair] `shouldBe` False

main :: IO ()
main = hspec spec
