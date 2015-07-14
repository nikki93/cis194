{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM

class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e


eval :: ExprT -> Integer
eval (Lit i) = i
eval (ExprT.Add e f) = eval e + eval f
eval (ExprT.Mul e f) = eval e * eval f

instance Expr ExprT where
  lit = Lit
  add = ExprT.Add
  mul = ExprT.Mul

evalStr :: String -> Maybe Integer
evalStr s = maybe Nothing (Just . eval) $ parseExp Lit ExprT.Add ExprT.Mul s
            -- compiler warnings suggest using <$> above, but I don't really
            -- know about that yet


instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)


instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)


newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax m) (MinMax n) = MinMax (max m n)
  mul (MinMax m) (MinMax n) = MinMax (min m n)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 m) (Mod7 n) = Mod7 ((m + n) `mod` 7)
  mul (Mod7 m) (Mod7 n) = Mod7 ((m * n) `mod` 7)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


instance Expr Program where
  lit = (: []) . PushI
  add = ((++ [StackVM.Add]) .) . (++)
  mul = ((++ [StackVM.Mul]) .) . (++)

compile :: String -> Maybe Program
compile = parseExp lit add mul

