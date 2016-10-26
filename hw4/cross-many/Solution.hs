module Solution where

solution :: [Int] -> [a] -> [a] -> ([a], [a])
solution is xs ys = solution' 0 is xs ys ([], [])

--xs or ys is empty
solution' :: Int -> [Int] -> [a] -> [a] -> ([a], [a]) -> ([a], [a])
solution' _ _ [] _ x = x
solution' _ _ _ [] x = x

-- xs and ys is not empty
solution' index (i:is) (x:xs) (y:ys) (xh, yh)
    | i < 0 = solution' index is (x:xs) (y:ys) (xh, yh)
    -- chage current 
    | index == i = solution' (index + 1) is xs ys (xh ++ [y], yh ++ [x])
    | otherwise = solution' (index + 1) (i:is) xs ys (xh ++ [x], yh ++ [y])

solution' index [] (x:xs) (y:ys) (xh, yh) = solution' (index + 1) [] xs ys (xh ++ [x], yh ++ [y])
