{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                   deriving (Show)



battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d)
  | a <= 1 || d <= 0 = return b
  | otherwise =
      replicateM (min 3 $ a - 1) die >>= \arolls ->
      replicateM (min 2 d) die >>= \drolls ->
      let matchups = zip (sort arolls) (sort drolls)
          awins = length $ filter (uncurry (>)) matchups
      in return $ Battlefield (a - (length matchups - awins)) (d - awins)

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
  | a <= 1 || d <= 0 = return b
  | otherwise = battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b =
  -- I was in an airport without internet and I couldn't figure out how to
  -- convert Int to Double, so I'm using this hack...
  liftM ((* 0.001) . fromIntegral . length . filter ((== 0) . defenders))
    $ replicateM 1000 $ invade b

