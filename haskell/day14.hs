import qualified Data.Map as M
import Data.Bifunctor(bimap)

getData = do
    input <- readFile "../data/input14.txt"
    let (start, rs) = bimap concat (drop 1) $ span (/= "") $ lines input
    let reacts = M.fromList $ map ((\[[a,b], _, [c]] -> ((a, b), ((a,c), (c,b)))) . words) rs
    let startPairs = zip start (drop 1 start)
    let startCount = M.fromListWith (+) (zip startPairs (repeat 1))
    pure (start, startCount, reacts)

addOrInsert k n = M.alter (Just . maybe n (+ n)) k

step rx m = M.foldrWithKey update M.empty m
    where
        update k n nm = case M.lookup k rx of
                            Just (a, b) -> addOrInsert a n (addOrInsert b n nm)
                            Nothing -> addOrInsert k n nm

census p = M.map (\n -> if odd n then (n+1) `div` 2 else n `div` 2) count
    where
        count =  M.foldrWithKey (\(a, b) n m -> addOrInsert a n (addOrInsert b n m)) M.empty p

solve p = maximum x - minimum x
    where
        x = M.elems (census p)

main = do
    (start, counts, rx) <- getData
    let evol = iterate (step rx) counts
    print (solve $ evol !! 10)
    print (solve $ evol !! 40)