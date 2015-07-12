module Main where

main :: IO ()
main = putStrLn "hello, world"

skips :: [a] -> [[a]]
skips xs = map skip [1 .. length xs]
      where skip n = map snd . filter ((== 0) . (`mod` n) . fst) $ zip [1..] xs

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_, m, _) -> m) $ filter (\(x, y, z) -> x < y && y > z)
                    $ zip3 xs (tail xs) (drop 2 xs)

histogram :: [Integer] -> String
histogram ns = concatMap row (reverse [1 .. maximum counts])
                ++ "==========\n0123456789\n"
          where counts = map (\d -> length $ filter (== d) ns) [0..9]
                row n = map (\c -> if c >= n then '*' else ' ') counts ++ "\n"

