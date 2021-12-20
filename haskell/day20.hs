module Main where

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Bifunctor
import Data.Maybe

default (Int, Double)

getData = do
    input <- readFile "../data/input20.txt"
    let (x:xs) = lines input
    let lookup = foldr (\(c,i) s -> if c == '#' then S.insert i s else s) S.empty $ zip x [0..]
    let ys = map (zip [0..]) (drop 1 xs)
    let flat = concat $ zipWith (\l i -> map (\(a, b) -> ((a, i), b)) l) ys [0..]
    let points = foldr (\(xy,c) m -> M.insert xy (c == '#') m) M.empty  flat
    pure (lookup, points)

fromBinary = foldl (\a b -> a*2+b) 0

hood = [ (x,y) | y <- [-1..1], x <- [-1..1]]

environ (x,y) = map (bimap (+x) (+y)) hood 

chunk m (x,y) c = fromBinary $ map (\(a,b) -> if fromMaybe c $ M.lookup (x+a, y+b) m then 1 else 0) hood

evolve table (m,c) = (nm, farColor)
    where
        nm = S.foldr (\xy ns ->  M.insert xy (S.member (chunk m xy c) table) ns) M.empty shood
        shood = S.foldr (\xy ns-> S.union ns $ S.fromList (environ xy)) S.empty $ M.keysSet m
        farColor = S.member (chunk m (100000,100000) c) table

main = do
    (lookup, s) <- getData
    let evol = evolve lookup
    let xs = iterate evol (s, False)
    print $ M.size $ M.filter id (fst $ xs !! 2)
    print $ M.size $ M.filter id (fst $ xs !! 50)