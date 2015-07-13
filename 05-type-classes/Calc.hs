module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e f) = eval e + eval f
eval (Mul e f) = eval e * eval f

evalStr :: String -> Maybe Integer
evalStr s = maybe Nothing (Just . eval) $ parseExp Lit Add Mul s
            -- compiler warnings suggest using <$> above, but I don't really
            -- know about that yet

