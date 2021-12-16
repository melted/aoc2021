import Data.List
import Data.Bifunctor

getData = do
    input <- readFile "../data/input16.txt"
    pure $ concatMap toBinary input

data Packet = Literal Int Int | Operator Int OpType [Packet] deriving (Show)

data OpType = Sum | Product | Minimum | Maximum | Gt | Lt | Equal deriving (Show)

toBinary :: Char -> [Int]
toBinary '0' = [0,0,0,0]
toBinary '1' = [0,0,0,1]
toBinary '2' = [0,0,1,0]
toBinary '3' = [0,0,1,1]
toBinary '4' = [0,1,0,0]
toBinary '5' = [0,1,0,1]
toBinary '6' = [0,1,1,0]
toBinary '7' = [0,1,1,1]
toBinary '8' = [1,0,0,0]
toBinary '9' = [1,0,0,1]
toBinary 'A' = [1,0,1,0]
toBinary 'B' = [1,0,1,1]
toBinary 'C' = [1,1,0,0]
toBinary 'D' = [1,1,0,1]
toBinary 'E' = [1,1,1,0]
toBinary 'F' = [1,1,1,1]
toBinary _ = []

fromBinary = foldl (\a b -> a*2+b) 0

getVal n xs = first fromBinary $ splitAt n xs

parsePacket xs = case typ of
                    4 -> parseLiteral version 0 zs
                    x -> parseOperator version x zs
    where
        (version, ys) = getVal 3 xs
        (typ, zs) = getVal 3 ys

parseOpType 0 = Sum
parseOpType 1 = Product
parseOpType 2 = Minimum
parseOpType 3 = Maximum
parseOpType 5 = Gt
parseOpType 6 = Lt
parseOpType 7 = Equal
parseOpType _ = error "bad op"

parseOperator ver typ (0:xs) = (Operator ver (parseOpType typ) (parsePackets ss), zs)
    where
         (sublen, ys) = getVal 15 xs
         (ss, zs) = splitAt sublen ys
parseOperator ver typ (1:xs) = (Operator ver (parseOpType typ) subpackets, zs)
    where
        (count, ys) = getVal 11 xs
        (subpackets, zs) = parseSubPackets count ys
parseOperator _ _ _ = error "Can't happen"

parseLiteral ver acc (0:xs) = let (v, zs) = getVal 4 xs in (Literal ver (acc*16+v), zs)
parseLiteral ver acc (1:xs) = let (v, zs) = getVal 4 xs in parseLiteral ver (acc*16+v) zs
parseLiteral _ _ _ = error "Can't happen"

parsePackets xs | length xs < 6 = []
parsePackets xs = let (p, ys) = parsePacket xs in p:parsePackets ys

parseSubPackets 0 xs = ([], xs)
parseSubPackets n xs = (p:ps, zs)
    where
        (p, ys) = parsePacket xs
        (ps, zs) = parseSubPackets (n-1) ys

versions (Literal v _) = v
versions (Operator v _ sp) = v + sum (map versions sp)

eval (Literal _ n) = n
eval (Operator _ Sum p) = sum (map eval p)
eval (Operator _ Product p) = product (map eval p)
eval (Operator _ Maximum p) = maximum (map eval p)
eval (Operator _ Minimum p) = minimum (map eval p)
eval (Operator _ Gt [a, b]) = if eval a > eval b then 1 else 0
eval (Operator _ Lt [a, b]) = if eval a < eval b then 1 else 0
eval (Operator _ Equal [a, b]) = if eval a == eval b then 1 else 0
eval o = error $ "unhandled packet " ++ show o

main = do
    d <- getData
    print (versions $ fst $ parsePacket d)
    print (eval $ fst $ parsePacket d)