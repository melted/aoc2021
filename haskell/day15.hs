module Main where
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Char(ord)
import Data.Maybe ( isJust )

getData = do
    input <- readFile "../data/input15.txt"
    let xs = lines input
    let heights = map (map (\c -> ord c - 48)) xs
    let ys = map (zip [0..]) heights
    let flat = concat $ zipWith (\l i -> map (\(a, b) -> ((a, i), b)) l) ys [0..]
    pure (length ys-1, length xs-1, M.fromList flat)

neighbors m (x, y) = filter (isJust . (m M.!?)) [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]

flow m cm = flow' cm (M.keysSet cm)
    where
        mini cm p  = minimum (map (cm M.!) $ neighbors m p)
        improvable cm p = mini cm p < (cm M.! p - (m M.! p))
        flow' nm points | S.size points == 0 = nm
        flow' nm points = flow' updated next
            where
                check = filter (improvable nm) $ S.toList points
                next = S.fromList $ concatMap (neighbors m) check
                updated =  foldr (\p -> M.insert p (m M.! p + mini nm p)) nm check

costMap m start = flow m $ costMap' M.empty (S.singleton start)
    where
        costMap' cm todo | S.size todo == 0 = cm
        costMap' cm todo = costMap' updated next
            where
                nb (x, y) = filter (isJust . (m M.!?)) [(x+1, y), (x, y+1)]
                cost (x,y) = case (M.lookup (x,y) m, M.lookup (x-1,y) cm, M.lookup (x, y-1) cm) of
                                (Just c, Nothing, Nothing) -> 0
                                (Just c, Just a, Nothing) -> c+a
                                (Just c, Nothing, Just a) -> c+a
                                (Just c, Just a, Just b) -> c + min a b
                                _ -> error $ "Can't happen" ++ show (x,y)
                next = S.fromList $ concatMap nb $ S.toList todo
                updated = S.foldr (\ p nm -> M.insert p (cost p) nm) cm todo

expandMap m = M.unions [ create x y | x <- [0..4], y <- [0..4]]
    where
        create x y = M.foldrWithKey (\(a, b) v nm ->
            M.insert (x*100+a, y*100+b) ((v+x+y-1) `mod` 9+1) nm) M.empty m

main = do
    (x, y, d) <- getData
    let cm = costMap d (0,0)
    print (cm M.! (x,y))
    let z = expandMap d
    let zm = costMap z (0,0)
    print (zm M.! (499,499))