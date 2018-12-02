module Day01(day01a) where

import System.FilePath
import System.IO

getNumList :: IO [Integer]
getNumList =
    map (read . dropWhile (=='+')) . lines <$> readFile path
    where path = "data" </> "day01a.input.txt"

day01a :: IO ()
day01a = getNumList >>= print . sum