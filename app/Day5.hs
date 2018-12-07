#!/usr/bin/env stack
-- stack --install-ghc runghc

module Day5 where

import Data.Char (toLower, isUpper, isLower)
import Data.Time (getCurrentTime, diffUTCTime)

-- Solution code

polymerReaction :: String -> String
polymerReaction x = foldr step "" x
  where
    step x (y:ys) | oppositePolarity x y = ys
    step x ys = x : ys

oppositePolarity :: Char -> Char -> Bool
oppositePolarity a b = a /= b && toLower a == toLower b

stripChar :: Char -> String -> String
stripChar c str = filter (\x -> toLower x /= c) str

partB :: String -> Int
partB p = minimum $ map (length . polymerReaction) (strippedPolymers p)
  where
    strippedPolymers p = [ stripChar c p | c <- ['a'..'z'] ]

main :: IO ()
main = do
    polymers <- readFile "day5.txt"

    start <- getCurrentTime
    putStr "Part 1: "
    putStrLn $ show $ length $ (polymerReaction polymers)
    end <- getCurrentTime
    print (diffUTCTime end start)

    start <- getCurrentTime

    putStr "Part 2: "
    putStrLn $ show $ partB polymers

    end <- getCurrentTime
    print (diffUTCTime end start)
