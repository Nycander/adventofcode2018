#!/usr/bin/env stack
-- stack --install-ghc runghc

import Data.Char
import Data.List
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set


removeInitialChar :: Char -> String -> String
removeInitialChar char str
    | head str == char = tail str
    | otherwise        = str

intsFromStr :: String -> [Int]
intsFromStr fileContent =
    map readInt (linesWithoutPlus fileContent)
    where
        readInt = read :: String -> Int
        removePlus = removeInitialChar '+'
        linesWithoutPlus c = map removePlus (lines c)

allFrequencies :: [Int] -> [Int]
allFrequencies freqs = scanl (+) 0 freqs

firstRepeat :: (Ord a) => [a] -> Maybe a
firstRepeat = run Set.empty
    where
        run s (x:xs)
            | x `Set.member` s = Just x
            | otherwise        = run (x `Set.insert` s) xs
        run _ [] = Nothing

day01a :: String -> Int
day01a fileContent = sum $ intsFromStr $ fileContent

day01b :: String -> Maybe Int
day01b fileContent = firstRepeat $ allFrequencies $ cycle $ (intsFromStr fileContent)

main :: IO ()
main = do
    fileContent <- readFile "day1.txt"
-- Step 1:
--     putStrLn $ show $ day01a fileContent
    putStrLn $ show $ day01b $ fileContent



