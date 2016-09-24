solution :: Integer -> Integer -> Integer
solution b n
    | n == 0 = 1
    | n `mod` 2 == 0 = (solution b (n `div` 2))^2
    | n `mod` 2 == 1 = b * solution b (n - 1)
