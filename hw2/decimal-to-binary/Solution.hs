solution :: [Char] -> [Char]
solution "" = "0"
solution "0" = "0"
solution x = solution' (read x :: Int)

solution' :: Int -> [Char]
solution' x
    | x == 0 = []
    | otherwise = solution' (x `div` 2) ++ show (x `mod` 2)
