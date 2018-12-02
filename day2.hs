#!/usr/bin/env stack
-- stack --install-ghc runghc

import Data.List

countLinesWithCharCount :: Int -> String -> Int
countLinesWithCharCount n = length . filter (hasCharCountOf n) . lines
    where
        hasCharCountOf n = any (\x -> length x == n) . groupBy (==) . sort

day02a :: String -> Int
day02a f = ((count 2 f) * (count 3 f))
    where
        count = countLinesWithCharCount

diffCount :: (Eq a) => [(a, a)] -> Int
diffCount = length . filter (\x -> x == False) . map (\p -> fst p == snd p)

commonLetters :: [String] -> String
commonLetters (x1:x2:xs) = map (\p -> fst p) (filter (\p -> fst p == snd p) (zip x1 x2))

-- Find the two lines which differ by 1
day02b :: String -> String
day02b fileContent = commonLetters $ take 2 $ filter (\a -> (any (diffByOne a) (lines fileContent))) $ lines fileContent
    where
        diffByOne a b = diffCount (zip a b) == 1

main :: IO ()
main = do
    fileContent <- readFile "day2.txt"

    putStr "Part 1: "
    putStrLn $ show $ day02a fileContent

    putStr "Part 2:"
    putStrLn $ show $ day02b fileContent
