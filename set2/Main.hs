{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.List (maximum, product)
import MCPrelude
import System.IO (IO, print)

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

instance Eq a => Eq (Maybe a) where
    (Just x) == (Just y) = x == y
    Nothing == Nothing = True
    _ == _ = False

headMay :: [a] -> Maybe a
headMay (h : _) = Just h
headMay _ = Nothing

tailMay :: [a] -> Maybe [a]
tailMay (_ : t) = Just t
tailMay _ = Nothing

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay target ((a, b) : xs)
    | target == a = Just b
    | otherwise = lookupMay target xs
lookupMay _ _ = Nothing

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay num divisor
    | divisor == 0 = Nothing
    | otherwise = Just $ num / divisor

maximumMay :: Ord a => [a] -> Maybe a
maximumMay (x : xs) = Just $ helper x xs
    where
        helper maxY (y : ys) = if y > maxY then helper y ys else helper maxY ys
        helper maxY _ = maxY
maximumMay _ = Nothing

minimumMay :: Ord a => [a] -> Maybe a
minimumMay (x : xs) = Just $ helper x xs
    where
        helper minY (y : ys) = if y < minY then helper y ys else helper minY ys
        helper minY _ = minY
minimumMay _ = Nothing

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd key =
    case lookupMay key gd of
        Nothing -> Nothing
        Just xs -> case tailMay xs of
                        Nothing -> Nothing
                        Just t -> case maximumMay t of
                                    Nothing -> Nothing
                                    Just maxX -> case headMay xs of
                                                    Nothing -> Nothing
                                                    Just h -> divMay (fromIntegral maxX) (fromIntegral h)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f (Just a) = f a
chain _ Nothing = Nothing

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link (Just a) f = f a
link Nothing _ = Nothing

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd key =
    lookupMay key gd `link` \xs ->
    tailMay xs `link` \t ->
    maximumMay t `link` \maxX ->
    headMay xs `link` \h ->
    divMay (fromIntegral maxX) (fromIntegral h)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries values n0 n1 =
    lookupMay n0 values `link` \s0 ->
    lookupMay n1 values `link` \s1 ->
    Just (s0 + s1)

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f mba mbb =
    mba `link` \a ->
    mbb `link` \b ->
    mkMaybe (f a b)

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 values n0 n1 = yLink (+) (lookupMay n0 values) (lookupMay n1 values)

mkMaybe :: a -> Maybe a
mkMaybe = Just

tailProd :: Num a => [a] -> Maybe a
tailProd as =
    tailMay as `link` \t ->
    mkMaybe (product t)

tailSum :: Num a => [a] -> Maybe a
tailSum as =
    tailMay as `link` \t ->
    mkMaybe (sum t)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f ma =
    ma `link` \a ->
    mkMaybe (f a)

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 as = transMaybe product (tailMay as)

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 as = transMaybe sum (tailMay as)

tailMax :: Ord a => [a] -> (Maybe (Maybe a))
tailMax as = transMaybe maximumMay (tailMay as)

tailMin :: Ord a => [a] -> (Maybe (Maybe a))
tailMin as = transMaybe minimumMay (tailMay as)

combine :: Maybe (Maybe a) -> Maybe a
combine mma = mma `link` id

main :: IO ()
main = do
    let x = Just "hello"
    print x

    print $ queryGreek greekDataA "alpha" == Just 2.0
    print $ queryGreek greekDataA "beta" == Nothing
    print $ queryGreek greekDataA "gamma" == Just 3.3333333333333335
    print $ queryGreek greekDataA "delta" == Nothing
    print $ queryGreek greekDataA "zeta" == Nothing

    print $ queryGreek greekDataB "rho" == Nothing
    print $ queryGreek greekDataB "phi" == Just 0.24528301886792453
    print $ queryGreek greekDataB "chi" == Just 9.095238095238095
    print $ queryGreek greekDataB "psi" == Nothing
    print $ queryGreek greekDataB "omega" == Just 24.0

    print $ queryGreek2 greekDataA "alpha" == Just 2.0
    print $ queryGreek2 greekDataA "beta" == Nothing
    print $ queryGreek2 greekDataA "gamma" == Just 3.3333333333333335
    print $ queryGreek2 greekDataA "delta" == Nothing
    print $ queryGreek2 greekDataA "zeta" == Nothing

    print $ queryGreek2 greekDataB "rho" == Nothing
    print $ queryGreek2 greekDataB "phi" == Just 0.24528301886792453
    print $ queryGreek2 greekDataB "chi" == Just 9.095238095238095
    print $ queryGreek2 greekDataB "psi" == Nothing
    print $ queryGreek2 greekDataB "omega" == Just 24.0

    print $ addSalaries salaries "alice" "bob"
    print $ addSalaries2 salaries "alice" "bob"

    print $ tailProd [100, 10, 20, 30]
    print $ tailSum [100, 10, 20, 30]
