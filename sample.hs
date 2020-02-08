module Sample where

import           Control.Monad.Writer

gcdReverse' :: Int -> Int -> Writer [String] Int
gcdReverse' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse' b (a `mod` b)
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)

traceGcdReverse :: Writer [String] Int
traceGcdReverse = do -- gcdReverse 3 2
  result <- do -- gcdReverse 2 1
    result <- do -- gcdReverse 1 0
      tell ["Finished with 0"]
      return (1 :: Int)
    tell ["2 mod 1 = 0"]
    return result
  tell ["3 mod 2 = 1"]
  return result

traceGcd' :: Writer [String] Int
traceGcd' = do -- gcdReverse 3 2
  tell ["3 mod 2 = 1"]
  do -- gcdReverse 2 1
    tell ["2 mod 1 = 0"]
    do  -- gcdReverse 1 0
      tell ["Finished with 0"]
      return (1 :: Int)

