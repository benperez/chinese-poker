module Util (combinations
            , deck
            , allPossibleHands
            ) where

import Control.Monad (filterM)
import Data.List (sort, group)

import Hand (Suit(..), Rank(..), Card, Hand(Hand), HandType, extractHandType)

combinations :: Int -> [a] -> [[a]]
combinations m xs = combsBySize xs !! m
  where
    combsBySize = foldr f ([[]] : repeat [])
    f x next = zipWith (++) (map (map (x:)) ([]:next)) next

deck :: [Card]
deck = [(rank, suit) | rank <- [Two .. Ace], suit <- [Diamonds .. Spades]]

--Warning, this has 2,598,960 elements
allPossibleHands :: [Hand]
allPossibleHands = map (\[a, b, c, d, e] -> Hand a b c d e) $ combinations 5 deck

handfrequencies :: [(HandType, Int)]
handfrequencies = map (\hts -> (head hts, length hts)) .
                  group .
                  sort .
                  map (fst . extractHandType) $ allPossibleHands
