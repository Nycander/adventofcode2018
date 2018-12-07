#!/usr/bin/env stack
-- stack --install-ghc runghc

module Day7 where

import Data.Char (toLower, isUpper, isLower)
import Data.Time (getCurrentTime, diffUTCTime)

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
      | otherwise     = let next = findNext deps visited
                        in go (Map.delete next deps) (next : path) (Set.insert next visited)

findNext :: Deps -> Set.Set Char -> Char
findNext deps visited =
  let
    options = Map.filter Set.null (Map.map (\x -> Set.difference x (x `Set.intersection` visited)) deps)
  in
    minimum $ Map.keys options


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
    putStrLn ""

    end <- getCurrentTime
    print (diffUTCTime end start)
