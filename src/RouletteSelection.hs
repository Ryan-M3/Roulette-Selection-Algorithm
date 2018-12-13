{- |
Module      : Roulette Selection
Description : Roulette Wheel Algorithm
License     : MIT
Maintainer  : Ryan McNamara <gn341ram@gmail.com>
Portability : Tested on Linux

Based on the paper found here: https://arxiv.org/abs/1109.3627

Basically, everyone  who tries to select  an item from a  list of
items with n probability is doing it wrong. The naive approach is
to iterate  through every  every item until  you hit  the correct
probability range. But you can  get much better amortized time if
you  select an  item  at  random and  reject  it  with a  certain
probability.
-}

{-# LANGUAGE ViewPatterns #-}

module RouletteSelection
    ( Item (..)
    , RWheel (..)
    , enqueue
    , dequeue
    , get
    , del
    )
where

import System.Random
import Data.List

data Item a = Item { value :: a
                   , prob  :: Double
                   } deriving (Show, Eq)

data RWheel a = RWheel { total ::  Double
                       , maxWt ::  Double
                       , items :: [Item a]
                       } deriving (Show, Eq)

-- |Add an item to the Roulette  Wheel so that it may be selected
-- later.
enqueue :: Eq a => RWheel a -> Item a -> RWheel a
enqueue wheel item = wheel{ total = total wheel + prob item
                          , maxWt = max (maxWt wheel) (prob item)
                          , items = item : items wheel
                          }

geoSeries :: Double -> Double -> Double
geoSeries q len = sum [q**i | i <- [0..len]]

-- |Select an  item with  the probability  equal to  it's weight.
-- Does not remove an item from the queue                       .
get :: Eq a => RWheel a -> StdGen -> Double -> (Item a, StdGen)
get (items -> [ ]) _ _ = error "Can't get item from empty list."
get (items -> [x]) g _ = (x, g)
get wheel g attempt =
    let a = 0
        z = (length . items) wheel - 1
        (rndIdx, g' ) = randomR (a  , z  ) g
        (rndPct, g'') = randomR (0.0, 1.0) g'
     in if   getP rndIdx wheel > rndPct
        then (items wheel !! rndIdx, g'')
        else get wheel g'' (attempt + 1)

del :: Eq a => RWheel a -> Item a -> RWheel a
del wheel item = wheel{ items = delete item $ items wheel }

-- |Select  and remove  an  item  from the  queue  with a  weight
-- proportional to it's probability.
dequeue :: Eq a => RWheel a -> StdGen -> (RWheel a, Item a, StdGen)
dequeue wheel g =
    let (item, g') = get wheel g 1
     in (del wheel item, item, g)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral $ length xs

-- |Magic formula to calculate the liklihood we should reject the
-- ith item.  The formula  is different  from the  probability of
-- selecting an item  because you have to take  into account that
-- you  will  draw  several  times  until  you  get  the  correct
-- probability.
getP :: Int -> RWheel a -> Double
getP i wheel = w / (n * wmax * geoSeries (1 - avg / wmax) 1000)
  where n    = fromIntegral . length . items $ wheel   -- number of items
        wmax = maxWt wheel                             -- largest weight
        w    = prob $ items wheel !! i                 -- weight of the ith item
        avg  = mean $ prob <$> items wheel             -- average weight
