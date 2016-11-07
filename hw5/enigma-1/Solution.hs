module Solution where
import Data.Char (ord, chr)
import Data.List

type Bit = Int

solution :: [Bit] -> String
solution = map (chr . bin2int) . chop9

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 (x:xs)
    | (sum y) `mod` 2 == x = y : chop9 (drop 8 xs) 
    | otherwise = error("failed parity bit checking")
    where y = take 8 xs