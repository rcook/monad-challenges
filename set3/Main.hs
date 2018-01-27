{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE RebindableSyntax  #-}

module Main (main) where

import MCPrelude
import System.IO (IO, print)

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f (a : as) bs = helper a bs ++ allCombs f as bs
    where
        helper x (y : ys) = f x y : helper x ys
        helper _ _ = []
allCombs f _ _ = []

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = allCombs (,)

data Card = Card Int String

instance Show Card where
    show (Card r s) = show r ++ s

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs =
    allCombs (\(a, b) c -> f a b c) (allCombs (,) as bs)

combStep :: [a -> b] -> [a] -> [b]
combStep (f : fs) as = map f as ++ combStep fs as
combStep _ _ = []

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs ys = [f] `combStep` xs `combStep` ys

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = map f xs `combStep` ys `combStep` zs

main :: IO ()
main = do
    print $ allPairs [1, 2] [3, 4]
    print $ allPairs [1..3] [6..8] == [(1, 6), (1, 7), (1, 8), (2, 6), (2, 7), (2, 8), (3, 6), (3, 7), (3, 8)]
    print $ allPairs cardRanks cardSuits == [(2, "H"), (2, "D"), (2, "C"), (2, "S"), (3, "H"), (3, "D"), (3, "C"), (3, "S"), (4, "H"), (4, "D"), (4, "C"), (4, "S"), (5, "H"), (5, "D"), (5, "C"), (5, "S")]
    print $ show (Card 2 "h") == "2h"
    print $ show (allCards cardRanks cardSuits) == "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"
    print $ allCombs3 (,,) [1, 2] [3, 4] [5, 6] == [(1, 3, 5), (1, 3, 6), (1, 4, 5), (1, 4, 6), (2, 3, 5), (2, 3, 6), (2, 4, 5), (2, 4, 6)]

    print $ allCombs' (,) [1..3] [6..8] == [(1, 6), (1, 7), (1, 8), (2, 6), (2, 7), (2, 8), (3, 6), (3, 7), (3, 8)]
    print $ allCombs3' (,,) [1, 2] [3, 4] [5, 6] == [(1, 3, 5), (1, 3, 6), (1, 4, 5), (1, 4, 6), (2, 3, 5), (2, 3, 6), (2, 4, 5), (2, 4, 6)]
