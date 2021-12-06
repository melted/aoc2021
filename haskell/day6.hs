import qualified Data.IntMap as I
import Data.Maybe
import Util

step :: I.IntMap Int -> I.IntMap Int
step m = foldr (\i -> I.insertWith (+) i (fromMaybe 0 $ m I.!? (i+1))) next [0..7]
    where
        next = I.insert 6 (fromMaybe 0 $m I.!? 0) $ I.singleton 8 (fromMaybe 0 $ m I.!? 0) 

solve :: Int -> I.IntMap Int -> Int
solve n start = sum $ I.elems $ foldr (\ i im -> step im) start [1..n]

census :: [Int] -> I.IntMap Int
census = foldr (\ x -> I.insertWith (+) x 1) I.empty

main :: IO ()
main = do
    a <- readFile "../data/input6.txt"
    let nums = map read $ split ',' a :: [Int]
    print (solve 80 (census nums))
    print (solve 256 (census nums))
