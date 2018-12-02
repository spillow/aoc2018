module Day01(day01a, day01b) where

import qualified Data.Set as S
import System.FilePath
import System.IO

getNumList :: IO [Integer]
getNumList =
    map (read . dropWhile (=='+')) . lines <$> readFile path
    where path = "data" </> "day01a.input.txt"

day01a :: IO ()
day01a = putStrLn "day01a:" >> getNumList >>= print . sum

day01b :: IO ()
day01b = do
    putStrLn "day01b:"
    nums <- getNumList
    let freqs = scanl (+) 0 $ cycle nums
    print $ go S.empty freqs
    where go _ [] = error "can't find!"
          go s (x:xs) | S.member x s = x
          go s (x:xs) = go (S.insert x s) xs