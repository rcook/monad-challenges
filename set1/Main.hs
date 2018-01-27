module Main (main) where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands =
    let (x0, s0) = rand (mkSeed 1)
        (x1, s1) = rand s0
        (x2, s2) = rand s1
        (x3, s3) = rand s2
        (x4, _) = rand s3
    in x0 : x1 : x2 : x3 : x4 : []

randLetter :: Gen Char
randLetter s = let (x, s') = rand s in (toLetter x, s')

randString3 :: String
randString3 =
    let (l0, s0) = randLetter (mkSeed 1)
        (l1, s1) = randLetter s0
        (l2, _) = randLetter s1
    in l0 : l1 : l2 : []

generalA :: (a -> b) -> Gen a -> Gen b
generalA f gen s =
    let (x, s') = gen s
    in (f x, s')

randEven :: Gen Integer
randEven = generalA (* 2) rand

randOdd :: Gen Integer
randOdd = generalA (\x -> x * 2 + 1) rand

randTen :: Gen Integer
randTen = generalA (* 10) rand

randPair :: Gen (Char, Integer)
randPair s =
    let (x0, s0) = rand s
        (x1, s1) = rand s0
    in ((toLetter x0, x1), s1)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair gena genb s =
    let (a, s0) = gena s
        (b, s1) = genb s0
    in ((a, b), s1)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f gena genb s =
    let (a, s0) = gena s
        (b, s1) = genb s0
    in (f a b, s1)

{-
repRandom :: [Gen a] -> Gen [a]
repRandom gs seed =
    foldl
        (\(items, s) g ->
            let (item, s') = g s
            in (items ++ [item], s'))
        ([], seed) gs
-}
repRandom :: [Gen a] -> Gen [a]
repRandom (g : gs) seed =
    let (a, s0) = g seed
        (as, s1) = repRandom gs s0
    in (a : as, s1)
repRandom _ seed = ([], seed)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo gena f seed =
    let (a, s0) = gena seed
        genb = f a
        (b, s1) = genb s0
    in (b, s1)

mkGen :: a -> Gen a
mkGen a seed = (a, seed)

main :: IO ()
main = do
    let xs = fiveRands
        prod = foldl (\acc x -> acc * x) 1 xs
    print $ prod == 8681089573064486461641871805074254223660

    print $ randString3

    let
        (x0, _) = randEven (mkSeed 1)
        (x1, _) = randOdd (mkSeed 1)
        (x2, _) = randTen (mkSeed 1)
    print $ x0 * x1 * x2 == 189908109902700

    print $ randPair (mkSeed 1)
    print $ generalPair randLetter rand (mkSeed 1)
    print $ generalB (,) randLetter rand (mkSeed 1)

    print $ repRandom (replicate 3 randLetter) (mkSeed 1)
