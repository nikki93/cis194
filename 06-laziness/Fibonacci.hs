{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)


data Stream a = Stream a (Stream a)

streamToList :: Stream a  -> [a]
streamToList (Stream e s) = e : streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat e = Stream e (streamRepeat e)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream e s) = Stream (f e) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream e s) t = Stream e $ interleaveStreams t s

ruler :: Stream Integer
ruler = ruler' 0
        where ruler' n = interleaveStreams (streamRepeat n) $ ruler' (n + 1)


data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  Matrix m11 m12 m21 m22 * Matrix n11 n12 n21 n22 = 
    Matrix (m11 * n11 + m12 * n21) (m11 * n12 + m12 * n22)
           (m21 * n11 + m22 * n21) (m21 * n12 + m22 * n22)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = f_n
         where Matrix _ f_n _ _ = e^n
               e = Matrix 1 1 1 0

