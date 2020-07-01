{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( distance,
    triggerChance,
    fieldGraphFun,
    plan,
    moveCost,
    Alarm (..),
    Coords,
  )
where

import Data.Graph.AStar (aStar)
import Data.HashSet (HashSet, fromList)
import Debug.Trace (traceId, traceShow, traceShowId)

type Coords = (Int, Int)

distance :: Coords -> Coords -> Double
distance (xFrom, yFrom) (xTo, yTo) = sqrt . fromIntegral $ (xTo - xFrom) ^ 2 + (yTo - yFrom) ^ 2

triggerChance :: Double -> Int -> Double
triggerChance l strength = exp $ negate ((l ** 2) / fromIntegral (strength ^ 2))

-- | Generate a "neighbors" function for a rectangle field of m × n square cells
fieldGraphFun :: Int -> Int -> (Coords -> HashSet Coords)
fieldGraphFun m n = fg'
  where
    xm = m - 1
    ym = n - 1
    fg' pos = fromList $ fg pos
    fg :: Coords -> [Coords]
    -- top left corner
    fg (0, 0) = [(1, 0), (0, 1)]
    -- top right corner
    fg (x, 0) | x == xm = [(x - 1, 0), (x, 1)]
    -- top border
    fg (x, 0) = [(x + 1, 0), (x - 1, 0), (x, 1)]
    -- bottom left corner
    fg (0, y) | y == ym = [(0, y - 1), (1, y)]
    -- left border
    fg (0, y) = [(0, y - 1), (0, y + 1), (1, y)]
    -- bottom right corner
    fg (x, y) | x == xm, y == ym = [(x - 1, y), (x, y - 1)]
    -- bottom border
    fg (x, y) | y == ym = [(x - 1, y), (x, y - 1), (x + 1, y)]
    -- right border
    fg (x, y) | x == xm = [(x - 1, y), (x, y - 1), (x, y + 1)]
    -- inside field
    fg (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

data Alarm = Alarm {position :: Coords, strength :: Int} deriving (Show)

moveCost :: [Alarm] -> Coords -> Coords -> Double
moveCost alarms from to = alarmChanceTotal + 2.0 
  -- Adding 2 to make sure the "distance" will be positive; Dijkstra algorithm 
  -- is known to fail on negative edge distances.
  where
    alarmChanceTotal = sum $ map (alarmChanceDelta from to) alarms
    alarmChanceDelta :: Coords -> Coords -> Alarm -> Double
    alarmChanceDelta from to Alarm {position, strength} =
      triggerChance (distance position to) strength - triggerChance (distance position from) strength

plan :: Int -> Int -> [Alarm] -> ([Coords], [Coords])
plan m n alarms = do
  let endPos = (m - 1, n - 1)
  let validAlarms = foldl alarmFilter [] alarms
  let (xm, ym) = endPos
  let diamondPos = (xm `div` 2, ym `div` 2) :: Coords
  let Just gettingIn = aStar (fieldGraphFun m n) (moveCost validAlarms) (distance diamondPos) (diamondPos ==) (xm, 0)
  let Just getaway = aStar (fieldGraphFun m n) (moveCost validAlarms) (distance (0, ym)) ((0, ym) ==) diamondPos
  (gettingIn, getaway)
  where
    alarmFilter :: [Alarm] -> Alarm -> [Alarm]
    alarmFilter acc a@(Alarm (x, y) s)
      | x > 0, x < m, y > 0, y < n, s > 0 = a : acc
      | otherwise = acc
