import qualified Data.Set as S
import Data.Bifunctor (bimap, second)

getData = do
    input <- readFile "../data/input13.txt"
    let (points, folds) = second (drop 1) $ span (/= "") $ lines input
    let parsed = map (bimap read (read . drop 1) . span (/= ',')) points :: [(Int, Int)]
    let parsedFolds = map (bimap last (read . drop 1) . span (/= '=')) folds :: [(Char, Int)]
    pure (S.fromList parsed, parsedFolds)

applyFold p (dir, n) = S.map flop p
    where 
        flop (x, y) | dir =='y' && y > n = (x, n - (y - n))
        flop (x, y) | dir =='x' && x > n = (n - (x - n), y)
        flop (x, y) = (x, y)

render p = map (\y -> map (disp y)  [0..maxX]) [0..maxY]
    where
        maxY = maximum (map snd $ S.toList p)
        maxX = maximum (map fst $ S.toList p)
        disp y x = if S.member (x,y) p then '#' else '.'

main = do 
    (m, f) <- getData
    print (S.size $ applyFold m (head f))
    mapM_ putStrLn $ render $ foldl applyFold m f