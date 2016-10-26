module Solution where

import Data.List
solution :: Int -> [a] -> [a] -> ([a], [a])
solution i xs ys = 
    let (xa, xb) = splitAt i xs
        (ya, yb) = splitAt i ys
    in  (ya ++ xb, xa ++ yb)
