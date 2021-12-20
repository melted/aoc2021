module Main where
import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (find, intersectBy, sort, sortOn)
import Data.Bifunctor
import Util

vectorify [a,b,c] = (a,b,c)
vectorify _ = error "expect three"

getData = do
    input <- readFile "../data/input19.txt"
    let scanners = map (drop 1) $ split "" $ lines input
    let parse s = vectorify $ map read $ split ',' s :: (Int, Int, Int)
    pure $ map (map parse) scanners

distance (x, y, z) (a, b, c) = abs (x-a) + abs (y-b) + abs (z-c)

closest scanners scanner = (take 2 $ filter (> 0) $ sort $ map (distance scanner) scanners, scanner)

facings = [\(x,y,z) -> (x,y,z), \(x,y,z) -> (-x,y,-z), \(x,y,z) -> (y, -x, z),
           \(x,y,z) -> (-y,x,z), \(x,y,z) -> (z,y,-x), \(x,y,z) -> (-z, y, x)]

rotations = [\(x,y,z) -> (x,y,z), \(x,y,z) -> (x,z,-y), \(x,y,z) -> (x,-y,-z), \(x,y,z) -> (x,-z,y)]

transforms :: [(Int, Int, Int) -> (Int, Int, Int)]
transforms = [a . b | a <- facings, b <- rotations]

sorted x = sortOn (head . fst) $ map (closest x) x

offsets [] = []
offsets ((a,b):xs) = map (\(s,t) -> (offset s a, offset t b)) xs

valid xs = filter ok (offsets xs)
    where
        vals (x,y,z) = sort [abs x, abs y, abs z]
        ok (a,b) = vals a == vals b

sameDist xs = all (uncurry (==)) $ offsets xs

offset (x, y, z) (a, b, c) = (x-a, y-b, z-c)
add (x,y,z) (a,b,c) = (x+a, y+b, z+c)

normalize x y = if M.size zc < 12
                    then Nothing
                    else Just (fixup, os)
    where
        xc = M.fromList $ sorted x
        yc = M.fromList $ sorted y
        zc = M.intersectionWith (,) xc yc
        pairs = valid $ M.elems zc
        transformTest t = sameDist $ map (Data.Bifunctor.second t) pairs
        Just tr = find transformTest transforms
        (a, b) = head pairs
        os = offset a (tr b)
        fixup = map (add os . tr) y

check [] = (S.empty, [])
check (x:xs) = check' S.empty (S.singleton x) (S.fromList xs) []
    where
        check' done solved todo p | S.size todo == 0 = (S.union done $ S.fromList $concat solved, p)
        check' done solved todo p | S.size solved == 0 = error "no leads"
        check' done solved todo p = check' ndone nsolved remain (pos++p)
            where
                solve [] x (a,b,c) = (a, S.insert x b, c)
                solve (s:ss) x (a,b,c) = case normalize s x of
                                            Just (y, z) -> (S.insert y a, b, z:c)
                                            Nothing -> solve ss x (a,b,c)
                (nsolved, remain, pos) =  S.foldr (solve $ S.toList solved)(S.empty, S.empty, []) todo
                ndone = S.union done $ S.fromList $concat solved

main = do
    d <- getData
    let (s,p) = check d
    print $ S.size s
    print $ maximum (concatMap (\x -> map (distance x) p) p)