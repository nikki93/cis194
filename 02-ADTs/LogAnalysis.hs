module LogAnalysis where

import Log

-- unwords . words /= identity, so this could be better...
parseMessage :: String -> LogMessage
parseMessage m = parseSplit (words m)
             where parseSplit ("E" : e : t : ws) = LogMessage (Error (read e)) (read t) (unwords ws)
                   parseSplit ("I" : t : ws)  = LogMessage Info (read t) (unwords ws)
                   parseSplit ("W" : t : ws)  = LogMessage Warning (read t) (unwords ws)
                   parseSplit ws = Unknown (unwords ws)

parse :: String -> [LogMessage]
parse f = map parseMessage (lines f)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node l m'@(LogMessage _ t' _) r)
       | t < t' = Node (insert m l) m' r
       | otherwise = Node l m' (insert m r)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m : ms) = insert m (build ms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong mss = descs $ inOrder $ build $ severe mss
              where severe [] = []
                    severe (m@(LogMessage (Error s) _ _) : ms)
                           | s >= 50 = m : severe ms
                           | otherwise = severe ms
                    severe (_ : ms) = severe ms
                    descs [] = []
                    descs (LogMessage _ _ desc : ms) = desc : descs ms
