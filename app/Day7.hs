#!/usr/bin/env stack
-- stack --install-ghc runghc

module Day7 where

import Data.Char (toLower, isUpper, isLower, ord)
import Data.Time (getCurrentTime, diffUTCTime)

import Debug.Trace (trace)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Solution code

type Deps = Map.Map Char (Set.Set Char)

parse :: String -> [(Char, Char)]
parse = map (\line -> (line !! 36, line !! 5)) . lines

dependencies :: [(Char, Char)] -> Deps
dependencies c = (depending c) `Map.union` (independent c)
  where
    depending c = Map.fromListWith Set.union (map (\(a,b) -> (a, Set.singleton b)) c)
    -- Produce entries with empty sets for all keys, Map.union will prefer first arg anyway
    independent c = Map.fromList $ map (\(a,b) -> (b,Set.empty)) c

findPath :: Deps -> String
findPath deps = go deps "" Set.empty
  where
    go :: Deps -> String -> Set.Set Char -> String
    go deps path visited
      | Map.null deps = reverse path
      | otherwise     = let next = minimum $ findOptions deps visited
                        in go (Map.delete next deps) (next : path) (Set.insert next visited)

findOptions :: Deps -> Set.Set Char -> [Char]
findOptions deps visited = Map.keys $ Map.filter Set.null (Map.map (\x -> Set.difference x (x `Set.intersection` visited)) deps)

timeUntilDone :: Deps -> Int -> Int
timeUntilDone deps workers = loop deps Map.empty (take workers $ repeat 0) 0
  where
    loop :: Deps -> Map.Map Char Int -> [Int] -> Int -> Int
    loop deps unavailAt workers t
      | Map.null deps                                 = maximum workers
      | all (>t) workers                              = loop deps unavailAt workers (t+1)
      | null $ findOptions deps (visited unavailAt t) = loop deps unavailAt workers (t+1)
      | otherwise                                     =
          let next = minimum $ findOptions deps (visited unavailAt t)
          in loop
            (Map.delete next deps)
            (Map.insert next (timeToWork next t) unavailAt)
            (addWorkToWorker workers t next)
            t

    visited :: Map.Map Char Int -> Int -> Set.Set Char
    visited unavailAt t = (Set.fromList $ Map.keys $ Map.filter (<=t) unavailAt)

    addWorkToWorker :: [Int] -> Int -> Char -> [Int]
    addWorkToWorker workers t work =
        let
          working = (filter (>t) workers)
          idle = (filter (<=t) workers)
        in
          (timeToWork work t : (tail idle)) ++ working

    timeToWork :: Char -> Int -> Int
    timeToWork work t = ord work - ord 'A' + 1 + t + 60

main :: IO ()
main = do
    input <- readFile "day7.txt"

    let graph = parse input
    let deps = dependencies graph

    start <- getCurrentTime
    putStr "Part 1: "
    putStrLn $ findPath deps
    end <- getCurrentTime
    print (diffUTCTime end start)

    start <- getCurrentTime
    putStr "Part 2: "
    putStrLn $ show $ timeUntilDone deps 5
    end <- getCurrentTime
    print (diffUTCTime end start)
