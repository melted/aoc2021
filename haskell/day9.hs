module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char(ord)
import Data.Maybe (mapMaybe)
import Data.List(sortOn)

getData :: IO (M.Map (Int, Int) Int)
getData = do
    input <- readFile "../data/input9.txt"
    let xs = lines input
    let heights = map (map (\c -> ord c - 48)) xs
    let ys = map (zip [0..]) heights
    let flat = concat $ zipWith (\l i -> map (\(a, b) -> ((a, i), b)) l) ys [0..]
    pure $ M.fromList flat

neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

lowPoint m (k, z) = all (\p -> maybe True (> z) $ M.lookup p m) (neighbors k)

checkPoint m (k, v) = if lowPoint m (k, v) then v + 1 else 0

lowPoints m = filter (lowPoint m) (M.toList m)

basin m p = basin' S.empty [p]
    where
        basin' acc [] = acc
        basin' acc xs = basin' done newPoints
            where
                valid p = maybe False (< 9) $ M.lookup p m
                done = S.union acc (S.fromList xs)
                candidates = concatMap neighbors xs
                newPoints = filter (\p -> valid p && S.notMember p done) candidates 

basins m = sortOn negate $ map (S.size . basin m . fst) (lowPoints m)

main = do
    d <- getData
    print (sum $ map (checkPoint d) $ M.toList d)
    print (product $ take 3 $ basins d)