module Main where

main :: IO ()
main = putStrLn "hello, world"

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = product $ map (subtract 2) $ filter even xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ filter even $ takeWhile (/= 1)
              $ iterate (\x -> if even x then x `div` 2 else 3 * x + 1) n

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) = Node (1 + max lh rh) l x r
         where hn = length xs `div` 2
               l = foldTree $ take hn xs
               r = foldTree $ drop hn xs
               height t = case t of { Node h _ _ _ -> h; Leaf -> -1 }
               lh = height l
               rh = height r

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> f a : bs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) $ filter (not . (`elem` skip)) [1..n]
              where skip = [i + j + 2 * i * j | i <- [1..n], j <- [1..n]]

