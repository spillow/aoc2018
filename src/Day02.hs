{-# LANGUAGE ViewPatterns #-}

module Day02(day02a,day02b) where

import System.FilePath
import qualified Data.Map.Strict as M

type OccMap = M.Map Char Integer

getLines :: IO [String]
getLines =
    lines <$> readFile path
    where path = "data" </> "day02a.input.txt"

mapify :: String -> OccMap
mapify = foldr bump M.empty
    where bump c m@(M.lookup c -> Just x) = M.insert c (x + 1) m
          bump c m = M.insert c 1 m

day02a :: IO ()
day02a = do
    lines <- getLines
    let maps = map mapify lines
    putStrLn "day02a"
    print $ hashElt 2 maps * hashElt 3 maps
    where hashElt n maps = length $ filter (hasRepeats n) maps
          hasRepeats n m = elem n $ M.elems m

day02b :: IO ()
day02b = do
    lines <- getLines
    putStrLn "day02b"
    print $ stripped $ head $ editDistances lines
    where editDistances lines = [(x, y) | x <- lines, y <- lines, dist x y == 1]
          dist x y = length $ filter (uncurry (/=)) (zip x y)
          stripped (x,y) = map fst $ filter (uncurry (==)) (zip x y)