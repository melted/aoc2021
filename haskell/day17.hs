
hit (x, y) = x >= 117 && x <= 164 && y <= -89 && y >= -140

beyond (x, y) = x > 164 || y < -140

nextState ((x, y), (xv, yv)) = ((x+xv, y+yv), (max (xv-1) 0, yv-1))

trajectory (xi, yi) = takeWhile (not . beyond . fst) $ iterate nextState ((0,0), (xi, yi))

score v = any (hit . fst) $ trajectory v

allScore = [ (x, y) | x <- [0..164] , y <- [-140..140], score (x,y)]

main = do 
    print (length allScore)