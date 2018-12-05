#!/usr/bin/env stack
-- stack --install-ghc runghc

module Day5 where

import Data.Char (toLower, isUpper, isLower)
import Data.Time (getCurrentTime, diffUTCTime)

import Debug.Trace (trace)

-- Solution code

polymerMultiReaction :: String -> String
polymerMultiReaction x = if x' == x then x else polymerMultiReaction x'
  where x' = polymerReaction x

-- Single pass polymer reaction
polymerReaction :: String -> String
polymerReaction []         = []
polymerReaction (x1:[])    = [x1]
polymerReaction (x1:x2:xs)
  | oppositePolarity x1 x2 = polymerReaction xs
  | otherwise              = x1 : polymerReaction (x2:xs)

oppositePolarity :: Char -> Char -> Bool
oppositePolarity a b
  | toLower a /= toLower b = False
  | isUpper a && isLower b = True
  | isLower a && isUpper b = True
  | otherwise              = False

stripChar :: Char -> String -> String
stripChar c str = filter (\x -> toLower x /= c) str

partB :: String -> Int
partB p = minimum $ map (length . polymerMultiReaction) (strippedPolymers p)
  where
    strippedPolymers p = [ stripChar c p | c <- ['a'..'z'] ]

main :: IO ()
main = do
    polymers <- readFile "day5.txt"

    start <- getCurrentTime
    putStr "Part 1: "
    putStrLn $ show $ length $ (polymerMultiReaction polymers)
    end <- getCurrentTime
    print (diffUTCTime end start)

    start <- getCurrentTime

    let polymersLength = length polymers
    putStr "Part 2: "
    putStrLn $ show $ partB polymers

    end <- getCurrentTime
    print (diffUTCTime end start)
