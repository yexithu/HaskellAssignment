module Solution where

import Data.List

-- solution :: [Integer] -> Int -> [Integer]
-- solution _  0 = []
-- solution x k 
--     | (2 * k) <= l = solution' x k mid
--     | otherwise = take k $ sort x 
--     where l = length x
--           mid = l `div` 2

-- solution' :: [Integer] -> Int -> Int -> [Integer]
-- solution' x k mid = 
--     let (a, b) = splitAt mid x
--     in take k $ sort $ (solution a k) ++ (solution b k)

solution :: [Integer] -> Int -> [Integer]
solution x k = take k $ sort x
