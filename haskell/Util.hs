module Util where

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split el xs = b : split el (drop 1 ys)
    where
        (b, ys) = span (/= el) xs
