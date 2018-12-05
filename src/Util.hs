module Util
  (
    removeInitialChar,
    removeLastChar,
    tupleDelimitedBy
  ) where

import Data.Text (split, pack, unpack)

removeInitialChar :: Char -> String -> String
removeInitialChar char str
    | head str == char = tail str
    | otherwise        = str

removeLastChar :: Char -> String -> String
removeLastChar char str
    | last str == char = init str
    | otherwise        = str

tupleDelimitedBy :: Char -> String -> (Int, Int)
tupleDelimitedBy delim str = toTuple $ map (read . unpack) (split (==delim) (pack str))
  where
    toTuple [a,b] = (a, b)