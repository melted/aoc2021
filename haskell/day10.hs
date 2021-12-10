module Main where
import Data.Maybe(mapMaybe)
import Data.List(sort)

getData = do
    input <- readFile "../data/input10.txt"
    pure $ lines input

data Result = OK | Incomplete Int | Error Int deriving (Show)

opener ch = ch == '(' || ch == '[' || ch == '{' || ch == '<'

closer '(' = ')'
closer '[' = ']'
closer '{' = '}'
closer '<' = '>'

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

stackScore xs = foldl (\a ch -> a*5+val ch) 0 xs
    where
        val '(' = 1
        val '[' = 2
        val '{' = 3
        val '<' = 4

parse xs = parse' [] xs
    where
        parse' [] [] = OK
        parse' stk [] = Incomplete (stackScore stk)
        parse' stk (x:xs) | opener x = parse' (x:stk) xs
        parse' (s:stk) (x:xs) | x == closer s = parse' stk xs
        parse' _ (x:xs) = Error (score x)

errorScore (Error x) = x
errorScore _ = 0

incompleteScore (Incomplete x) = Just x
incompleteScore _ = Nothing

solve2 d = xs !! (length xs `div` 2)
    where
        xs = sort $ mapMaybe (incompleteScore . parse) d

main = do
    d <- getData
    print (sum $ map (errorScore . parse) d)
    print (solve2 d)