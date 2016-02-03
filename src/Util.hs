module Util (comb, deck, allPossibleHands) where

import Control.Monad (filterM)

import Hand (Suit(..), Rank(..), Card, Hand(Hand))

comb :: Int -> [a] -> [[a]]
comb m xs = combsBySize xs !! m
  where
    combsBySize = foldr f ([[]] : repeat [])
    f x next = zipWith (++) (map (map (x:)) ([]:next)) next

deck :: [Card]
deck = [(rank, suit) | rank <- [Two .. Ace], suit <- [Diamonds .. Spades]]

--Warning, this has 2,598,960 elements
allPossibleHands :: [Hand]
allPossibleHands = map (\[a, b, c, d, e] -> Hand a b c d e) $ comb 5 deck
