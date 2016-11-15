module Solution where
import Data.List
import Data.Char
solution :: [(Char, Int)] -> Int
solution [] = 8
solution x = solution' $ map charToInt x

solution' :: [(Int, Int)] -> Int
solution' x = let l = length x
                  board = [(a, b) | a <- [1,2..8], b <- [1,2..8]]
                  left = foldl (\z ele -> filter (isSafe ele) z) board x
              in (solutionInt l left) - l

charToInt :: (Char, Int) -> (Int, Int)
charToInt (a, b) = (ord(a) - ord('a') + 1, b) 

solutionInt :: Int -> [(Int, Int)] -> Int
solutionInt x [] = x
solutionInt x left = foldl (\z ele -> bfsearch x left z ele) 0 left
  
bfsearch :: Int -> [(Int, Int)] -> Int -> (Int, Int) -> Int
bfsearch _ _ 8 _ = 8
bfsearch x left z ele = let removed = filter (isSafe ele) left
                            y = solutionInt (x + 1) removed
                        in if y > z then y else z

isSafe :: (Int, Int) -> (Int, Int) -> Bool
isSafe (a, b) (c, d)
    | a == c = False
    | b == d = False
    | (abs (a - c)) == (abs (b - d)) = False
    | otherwise = True
