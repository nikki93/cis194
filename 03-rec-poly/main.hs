module Main where

main :: IO ()
main = putStrLn "hello, world"

skips :: [a] -> [[a]]
skips xs = map skip [1 .. length xs]
      where skip n = map snd . filter ((== 0) . (`mod` n) . fst) $ zip [1..] xs
