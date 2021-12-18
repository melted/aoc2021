import Data.Char

getData = do
    d <- readFile "../data/input18.txt"
    pure $ map (fst . parseVal) $ lines d

data SNum = Pair SNum SNum | Lit Int deriving (Show, Eq)

data Explode = Exploded SNum (Maybe Int) (Maybe Int) | Unchanged SNum

parseVal (x:xs) | isDigit x = (Lit (ord x - 48), xs)
parseVal ('[':xs) = (Pair val val2, drop 1 zs)
    where
        (val, ys) = parseVal xs
        (val2, zs) = parseVal (drop 1 ys)
parseVal _ = error "unexpected input"

addRight n (Lit x) = Lit (n+x)
addRight n (Pair x y) = Pair x (addRight n y)

addLeft n (Lit x) = Lit (n+x)
addLeft n (Pair x y) = Pair (addLeft n x) y

explode p = explode' p 0
    where
        explode' (Pair (Lit x) (Lit y)) n | n > 3 = Exploded (Lit 0) (Just x) (Just y)
        explode' (Pair l r) n = 
            case explode' l (n+1) of
                Unchanged y -> 
                    case explode' r (n+1) of
                        Unchanged z -> Unchanged (Pair y z)
                        Exploded m (Just a) b -> Exploded (Pair (addRight a l) m) Nothing b
                        Exploded m Nothing b -> Exploded (Pair l m) Nothing b
                Exploded m a (Just b) -> Exploded (Pair m (addLeft b r)) a Nothing
                Exploded m a Nothing -> Exploded (Pair m r) a Nothing
        explode' x _ = Unchanged x

split (Lit n) | n > 9 = let f = fromInteger (toInteger n)/2 
                                in Right (Pair (Lit (floor f)) (Lit (ceiling f)))
split (Lit n) = Left (Lit n)
split (Pair l r) = 
    case split l of
        Left x -> case split r of
                        Left y -> Left (Pair x y)
                        Right z -> Right (Pair x z)
        Right x -> Right (Pair x r)

reduce p = case explode p of
                Exploded np _ _ -> reduce np
                Unchanged _ -> case split p of
                                    Right np -> reduce np
                                    Left np -> np

add x y = reduce (Pair x y)

magnitude (Lit n) = n
magnitude (Pair a b) = 3*magnitude a+2*magnitude b

main = do
    d <- getData
    print (magnitude $ foldl1 add d)
    print (maximum [ magnitude $ add x y | x <- d, y <- d ])