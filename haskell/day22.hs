import Data.Bifunctor
import Data.Char

data Block = Block (Int,Int) (Int, Int) (Int, Int) deriving (Show, Eq, Ord)

getData = do
    input <- readFile "../data/input22.txt"
    let d = map (span (/= ' ')) $ lines input
    let makeBlock [a,b,c,d,e,f] = Block (min a b,max a b) (min c d,max c d) (min e f, max e f)
    let parse s = makeBlock $ map read $ words $ map (\c -> if isDigit c || c == '-' then c else ' ') s
    pure $ map (bimap (== "on") parse) d

inside (x1,x2) (a1, a2) = not (x1 > a2 || x2 < a1)

intersect (Block x y z) (Block a b c) = inside x a && inside y b && inside z c

remaining block other | not $ intersect block other = [block]
remaining (Block (x1, x2) y z) o@(Block (a1, a2) b c) | x1 < a1 = Block (x1, a1-1) y z:remaining (Block (a1, x2) y z) o
remaining (Block (x1, x2) y z) o@(Block (a1, a2) b c) | x2 > a2 = Block (a2+1, x2) y z:remaining (Block (x1, a2) y z) o
remaining (Block x (y1, y2) z) o@(Block a (b1, b2) c) | y1 < b1 = Block x (y1, b1-1) z:remaining (Block x (b1, y2) z) o
remaining (Block x (y1, y2) z) o@(Block a (b1, b2) c) | y2 > b2 = Block x (b2+1, y2) z:remaining (Block x (y1, b2) z) o
remaining (Block x y (z1, z2)) o@(Block a b (c1, c2)) | z1 < c1 = Block x y (z1, c1-1):remaining (Block x y (c1, z2)) o
remaining (Block x y (z1, z2)) o@(Block a b (c1, c2)) | z2 > c2 = Block x y (c2+1, z2):remaining (Block x y (z1, c2)) o
remaining _ _ = []

volume (Block (x, x1) (y,y1) (z, z1)) = (x1-x+1)*(y1-y+1)*(z1-z+1)

check b blocks = foldr (\x acc -> remaining x b ++ acc) [] blocks

handle acc [] = acc
handle acc ((True,b):bs) = handle (b:check b acc) bs
handle acc ((False, b):bs) = handle (check b acc) bs

solve bs = sum $ map volume bs

bounded n (_, Block (x,_) _ _) = abs x <= n

main = do
    d <- getData
    print (solve $ handle [] $ filter (bounded 50) d)
    print (solve $ handle [] d)
