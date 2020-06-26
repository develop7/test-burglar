module Lib
  ( someFunc,
    distance,
    triggerProbability,
    fieldGraphFun,
    plan
  )
where

import Data.HashSet
import Data.Graph.AStar (aStar)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Coords = (Int, Int)

distance :: Coords -> Coords -> Double
distance (xFrom, yFrom) (xTo, yTo) = sqrt . realToFrac $ (xTo - xFrom) ^ 2 + (yTo - yFrom) ^ 2

triggerProbability :: Double -> Double -> Double
triggerProbability l strength = exp $ negate ((l ** 2) / (strength ** 2))

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

data Alarm = Alarm {position :: Coords, strength :: Int}

moveCostFun :: [Alarm] -> Coords -> Coords -> Double
moveCostFun alarms pos _ = 1.0

plan :: Int -> Int -> [Coords] -> [Coords]
plan m n alarms = do
  let exitPos = (m-1, n-1)
  let (xm, ym) = exitPos
  let diamondPos = ( xm `div` 2, ym `div` 2) :: Coords
  let Just gettingIn = aStar (fieldGraphFun m n) (moveCostFun []) (distance diamondPos) (diamondPos ==) (0,0)
  let Just getaway = aStar (fieldGraphFun m n) (moveCostFun []) (distance exitPos) (exitPos ==) diamondPos
  gettingIn ++ getaway
