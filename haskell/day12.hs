
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor(second)
import Data.Tuple(swap)
import Data.Char(isLower)

getData = do
    input <- readFile "../data/input12.txt"
    let edges = map (second (drop 1) . span (/= '-')) (lines input)
    let reversed = map swap edges
    let all = map (second (:[])) (edges++reversed)
    pure $ M.fromListWith (++) all

validPath part2 p = (census M.! "start") == 1 &&  count (> 2) == 0 
                        && count (== 2) < if part2 then 2 else 1
    where
        count f = M.size (M.filter f census)
        census = M.filterWithKey (\k a ->isLower $ head k) $ M.fromListWith (+) (zip p (repeat 1))

findPaths part2 from to m = findPaths' S.empty (S.singleton [from])
    where
        findPaths' done live | S.size live == 0 = done
        findPaths' done live = findPaths' updated next
            where
                finished = S.filter (\p -> head p == to) live
                updated = S.union done finished
                newPaths p = S.fromList $ filter (validPath part2) $
                                 maybe [] (map (:p)) (M.lookup (head p) m)
                next = S.foldr (S.union . newPaths) S.empty (live S.\\ finished)

main = do
    d <- getData
    print (S.size $ findPaths False "start" "end" d)
    print (S.size $ findPaths True "start" "end" d)
