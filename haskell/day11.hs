module Main where
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor(second)
import Data.Char(ord)

getData :: IO (M.Map (Int, Int) Int)
getData = do
    input <- readFile "../data/input11.txt"
    let xs = lines input
    let heights = map (map (\c -> ord c - 48)) xs
    let ys = map (zip [0..]) heights
    let flat = concat $ zipWith (\l i -> map (\(a, b) -> ((a, i), b)) l) ys [0..]
    pure $ M.fromList flat

neighbors (x,y) = [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], a /= x || b /= y]

flash m = flash' S.empty (readyToFlash S.empty m) m
    where
        readyToFlash done m = S.fromList (map fst $ M.toList $ M.filter (> 9) m) S.\\ done
        zeroFlashed flashed cm = M.mapWithKey (\k v -> if S.member k flashed then 0 else v) cm
        flash' flashed current cm | S.size current == 0 = (zeroFlashed flashed cm, S.size flashed)
        flash' flashed current cm = flash' done next updated
            where
                done = S.union flashed current
                updated = foldr (M.adjust (+ 1)) cm $ concatMap neighbors current
                next = readyToFlash done updated

step (m, n) = second (+n) $ flash (M.map (+1) m)

main = do
    d <- getData
    let (_, score) = iterate step (d, 0) !! 100
    print score
    let xs = takeWhile (not . all (== 0) . fst) $ iterate step (d,0)
    print (length xs)
