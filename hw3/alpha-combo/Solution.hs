module Solution where
import Data.Char

solution :: Integer -> [[Char]]
solution n
    | n == 0 = []
    | n <= 26 = foldl (++) [] (map (solution' n) [(ord 'a') .. (ord 'z')])
    | otherwise = []

solution' :: Integer -> Int -> [[Char]]
solution' n c
    | n > toInteger (ord 'z' - c + 1) = []
    | n == 0 = []
    | n == 1 = [[chr c]]
    | otherwise = (map (\xs -> (chr c) : xs) $ foldl (++) [] $ map (solution' $ n - 1) [(c + 1) .. (ord 'z')])