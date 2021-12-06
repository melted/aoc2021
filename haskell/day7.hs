import Util(split)

triangular n = n*(n+1) `div` 2

solve f xs = minimum $ map (cost f) [0..2000]
    where cost f x = sum $ map (\n -> f $ abs (n - x)) xs

main = do
    input <- readFile "../data/input7.txt"
    let depths = map read $ split ',' input :: [Int]
    print (solve id depths)
    print (solve triangular depths)