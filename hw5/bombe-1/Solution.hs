module Solution where
import Data.Char (ord, chr)
import Data.List

type Bit = Int

solution :: String -> [Bit]
solution = concat . map (make9 . int2bin . ord)

int2bin :: Int -> [Bit]
int2bin 0 = [] 
int2bin n =  n `mod` 2 : int2bin (n `div` 2) 

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

make9 :: [Bit] -> [Bit]
make9 bits = let x = make8 bits
             in (sum x) `mod` 2 : x    
