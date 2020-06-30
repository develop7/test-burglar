module Main where

import Data.List.Split (splitOn)
import Lib

main :: IO ()
main = do
  putStrLn "Enter space separated room length and width:"
  line <- getLine
  let (m : n : _) = map read (words line) :: [Int]
  putStrLn "Enter space separated alarm coordinates and sensivity, empty line to end (example: 2 4 3 means alarm on (2,4) of sensivity 3): "

  alarms <- readAlarms []
  let (getIn, getAway) = plan m n alarms
  putStrLn $ "Okay, here's the plan:\n1. Get in.\n2. Move through these exact cells - " ++ show ((m - 1, 0) : getIn)
  putStrLn $ "3. Grab the diamond\n4. Move towards the exit via these cells - " ++ show getAway
  putStrLn "5. ??????\n6. PROFIT"
  where
    parseCoords line = do
      let (x : y : strength : _) = map read $ words line :: [Int]
      Alarm (x, y) strength
    readAlarms alarms = do
      input <- getLine
      if input /= ""
        then readAlarms (parseCoords input : alarms)
        else return alarms
