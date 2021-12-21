module Main where

import qualified Data.Map as M

rolls = cycle [1..100]

play (x, p, r) die = (nx, p + nx, r+3)
    where
        roll = sum $ take 3 die
        nx = (x + roll - 1) `mod` 10 + 1

match (x1, p1, r1) (x2, p2, r2) die
  | b >= 1000 = p2*(c+r2)
  | e >= 1000 = p1*(f+r1)
  | otherwise = match (a,b,c) (d,e,f) (drop 6 die)
  where
      (a, b, c) = play (x1, p1, r1) die
      (d, e, f) = play (x2, p2, r2) (drop 3 die)

distro = [(3,1), (4,3), (5, 6), (6, 7), (7, 6), (8, 3), (9,1)]

update (x, p, x2, p2) n = let nx = (x + n - 1) `mod` 10 + 1 in (nx, p+nx, x2, p2)
update2 (x, p,x2, p2) n = let nx = (x2 + n - 1) `mod` 10 + 1 in (x, p, nx, p2+nx)
play2 d update = foldr spread M.empty distro
    where
        spread (v, n) nd = M.foldrWithKey (\k a dd -> M.insertWith (+) (update k v) (a*n) dd) nd d

match2 p1 p2 = match2' (M.singleton (p1,0,p2,0) 1) (0,0)
    where
        match2' d (w1, w2) | M.size d == 0 = max w1 w2
        match2' dist (w1, w2) = match2' ndist nw
            where
                (xs, m) = M.partitionWithKey (\(_,p,_,_) a -> p>=21 ) $ play2 dist update
                (ys, ndist) = M.partitionWithKey (\(_,_,_,p) a -> p>=21 ) $ play2 m update2
                nw = (w1 + sum (M.elems xs), w2 + sum (M.elems ys))

main = do
    print $ match (4,0,0) (6,0,0) rolls
    print $ match2 4 6
