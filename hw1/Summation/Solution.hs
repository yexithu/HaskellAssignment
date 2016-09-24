sumOfCubes :: Integer -> Integer -> Integer
sumOfCubes m n
    | m > n = 0
    | otherwise = m^3 + sumOfCubes (m + 1) n

sumOfF :: (Integer -> Integer) -> Integer -> Integer -> Integer
sumOfF f m n
    | m > n = 0
    | otherwise = f m + (sumOfF f (m + 1) n)

sumAlongNext :: (Integer -> Integer) -> Integer -> (Integer -> Integer) -> Integer -> Integer
sumAlongNext f m next n
    | m > n = 0
    | otherwise = f m + sumAlongNext f (next m) next n
