module Main where

main :: IO ()
main = putStrLn "hello, world"

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3*n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n : ns) = n `div` 10 + n `mod` 10 + sumDigits ns

doubleEveryOtherFromStart :: [Integer] -> [Integer]
doubleEveryOtherFromStart [] = []
doubleEveryOtherFromStart ns@[_] = ns
doubleEveryOtherFromStart (x:y:xs) = x : (2*y) : doubleEveryOtherFromStart xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromStart . reverse

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
