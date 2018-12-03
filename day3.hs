#!/usr/bin/env stack
-- stack --install-ghc runghc

import Data.Text (split, pack, unpack)
import qualified Data.Set as Set

import Debug.Trace (trace)

 -- These are starting to become utils, create module?
removeInitialChar :: Char -> String -> String
removeInitialChar char str
    | head str == char = tail str
    | otherwise        = str

removeLastChar :: Char -> String -> String
removeLastChar char str
    | last str == char = init str
    | otherwise        = str

tupleDelimitedBy :: Char -> String -> (Int, Int)
tupleDelimitedBy delim str = toTuple $ map (readInt . unpack) (split (==delim) (pack str))
  where
    readInt = read :: String -> Int
    toTuple [a,b] = (a, b)

-- Claim and its parsing code

data Claim = Claim {
  claimId :: Int,
  start :: (Int, Int),
  size :: (Int, Int)
} deriving (Show)

parseClaim :: String -> Claim
parseClaim txt =
  let [claimId, _, start, size] = words txt
  in Claim {
    claimId = readInt $ removeInitialChar '#' claimId,
    start = tupleDelimitedBy ',' $ removeLastChar ':' start,
    size = tupleDelimitedBy 'x' size
  }
    where
      readInt = read :: String -> Int

-- Solution code!

fabric :: Claim -> Set.Set (Int, Int)
fabric c = let
  x = fst $ start c
  y = snd $ start c
  width = fst $ size c
  height = snd $ size c
  in Set.fromList [ (dx, dy) |
    dx <- [x..(x + width - 1)],
    dy <- [y..(y + height - 1)]
  ]

findOverlap :: [Claim] -> Set.Set (Int, Int)
findOverlap claims = snd $ foldl run (Set.empty, Set.empty) (map fabric claims)
  where
    run (seen, overlaps) set = (seen `Set.union` set, overlaps `Set.union` (seen `Set.intersection` set))

findIntactClaim :: [Claim] -> Claim
findIntactClaim claims = head $ filter hasDisjointFabric claims
  where
    hasDisjointFabric c = fabric c `Set.disjoint` overlap
    overlap = findOverlap claims

main :: IO ()
main = do
    fileContent <- readFile "day3.txt"
    let claims = map parseClaim (lines fileContent)

    putStr "Part 1: "
    putStrLn $ show $ length $ findOverlap claims

    putStr "Part 2: "
    putStrLn $ show $ findIntactClaim claims
