import qualified Data.Set as S

default (Int, Double)
getData = do
    input <- readFile "../data/input25.txt"
    let rows = lines input
    let height = length rows
    let width = length $ head rows
    let parse (x,y,c) (es,ss)
          | c == '>' = (S.insert (x,y) es, ss)
          | c == 'v' = (es, S.insert (x,y) ss)
          | otherwise = (es, ss)
    let coords = zipWith (\r y->zipWith (\c x -> (x,y,c)) r [0..]) rows [0..]
    let sc = foldr parse (S.empty, S.empty) $ concat coords
    pure (height, width, sc)

evolve (h,w,(es, ss)) = (diff,(h,w,(nes,nss)))
    where
        nx (x,y) = ((x+1) `mod` w,y)
        nes = S.map (\p -> let a = nx p in if S.member a es || S.member a ss then p else a) es
        ny (x,y) = (x, (y+1) `mod` h)
        nss = S.map (\p -> let a = ny p in if S.member a nes || S.member a ss then p else a) ss
        diff = S.size (S.difference es nes)+S.size (S.difference ss nss)

main = do
    d <- getData
    let r = iterate (\(s,d) -> evolve d) (1, d)
    let s = takeWhile ((> 0) . fst) r
    print $ length s