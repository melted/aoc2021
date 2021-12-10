import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable(find)
import Data.Bifunctor(second)

getData = do
    input <- readFile "../data/input8.txt"
    let xs = map words (lines input)
    let sets = map (map S.fromList) xs
    pure $ map (second (drop 1) . span (S.notMember '|')) sets


solve1 x = length (filter pred x)
    where
        pred x = let l = length x in l == 2 || l == 3 || l == 4 || l == 7

decipher :: [S.Set Char] -> M.Map (S.Set Char) Int
decipher xs = M.fromList [(one, 1), (seven, 7), (four, 4), (eight, 8), (three, 3),
                    (nine, 9), (zero, 0), (six, 6), (five, 5), (two, 2)]
    where
        segs n ys = find (\s -> S.size s == n) ys
        Just one =  segs 2 xs
        Just seven = segs 3 xs
        Just four = segs 4 xs
        Just eight = segs 7 xs
        Just three = find (\s -> S.size s == 5 && S.isSubsetOf seven s) xs
        Just nine = find (\s -> S.size s == 6 && S.isSubsetOf four s) xs
        Just zero = find (\s -> S.size s == 6 && S.isSubsetOf seven s && s /= nine) xs
        Just six = find (\s -> S.size s == 6 && s /= zero && s /= nine) xs
        Just five = find (\s -> S.size s == 5 && S.isSubsetOf s six) xs
        Just two = find (\s -> S.size s == 5 && s /= three && s /= five) xs

solve2 :: ([S.Set Char],[S.Set Char]) -> Int
solve2 (defs, disp) = foldl (\acc n -> acc*10 + (table M.! n)) 0 disp
                where
                    table = decipher defs
main = do
    input <- getData
    print (sum (map (solve1 . snd) input))
    print (sum (map solve2 input))
