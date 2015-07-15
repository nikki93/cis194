{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Data.Monoid

import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
j +++ Empty = j
Empty +++ j = j
j +++ k = Append (tag j <> tag k) j k

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ e) = Just e
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ j k) | i < sj = indexJ i j
                        | otherwise = indexJ (i - sj) k
                        where sj = getSize (size $ tag j)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n j | n <= 0 = j
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty -- n <= 0 case handled above, so we have n > 0
dropJ n (Append _ j k) | n < sj = dropJ n j +++ k
                       | otherwise = dropJ (n - sj) k
                       where sj = getSize (size $ tag j)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ n j | n >= getSize (size $ tag j) = j
takeJ n (Append _ j k) | n < sj = takeJ n j
                       | otherwise = j +++ takeJ (n - sj) k
                       where sj = getSize (size $ tag j)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ a) = a
  toString (Append _ j k) = toString j ++ toString k

  fromString = fromLines . lines
               where fromLines [] = Empty
                     fromLines ls = l +++ Single (scoreString m, 1) m +++ r
                                    where l = fromLines $ take hn ls
                                          r = fromLines $ drop (hn + 1) ls
                                          m = ls !! hn
                                          hn = length ls `div` 2

  line = indexJ

  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n + 1) b

  numLines = getSize . snd . tag
  value = getScore . fst . tag

main = runEditor editor $ (fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinList (Score, Size) String)

