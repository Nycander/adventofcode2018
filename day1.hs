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
        linesWithoutPlus = map removePlus . lines


day01a :: String -> Int
day01a = sum . intsFromStr

day01b :: String -> Maybe Int
day01b = firstRepeat . allFrequencies . intsFromStr
    where
        allFrequencies = scanl (+) 0 . cycle

firstRepeat :: (Ord a) => [a] -> Maybe a
firstRepeat = run Set.empty
    where
        run s (x:xs)
            | x `Set.member` s = Just x
            | otherwise        = run (x `Set.insert` s) xs
        run _ [] = Nothing

printLn :: (Show a) => a -> IO ()
printLn = putStrLn . show

main :: IO ()
main = do
    fileContent <- readFile "day1.txt"
    printLn $ day01a fileContent
    printLn $ day01b fileContent
