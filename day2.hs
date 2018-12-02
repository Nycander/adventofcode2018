#!/usr/bin/env stack
-- stack --install-ghc runghc

import Data.List

countLetters :: Int -> String -> Int
countLetters n = length . filter (hasCharCountOf n) . lines
    where
        hasCharCountOf n = any (\x -> length x == n) . groupBy (==) . sort

main :: IO ()
main = do
    fileContent <- readFile "day2.txt"

    putStrLn $ show $ ((countLetters 2 fileContent) * (countLetters 3 fileContent))
