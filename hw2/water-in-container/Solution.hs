solution :: Integer -> Integer -> Integer -> Integer


solution m n t = solution' [(0, 0, 0, 1), (0, 0, 0, 0)] m n t
solution' :: [(Integer, Integer, Integer, Integer)] -> Integer -> Integer -> Integer -> Integer

solution' ((times, a, b,_):xs) m n t
    | a == t || b == t = times

solution' ((times, a, b, 1):xs) m n t = solution' (xs ++ [(atob times a b m n)]) m n t
solution' ((times, a, b, 0):xs) m n t = solution' (xs ++ [(btoa times a b m n)]) m n t

atob :: Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer, Integer)
atob times a b m n
    | a == 0 = (times + 1, m, b, 1)
    | a > 0 && b == n =(times + 1, a, 0, 1)
    | a + b <= n =(times + 1, 0, a + b, 1)
    | a + b > n = (times + 1, a + b - n, n, 1)

btoa :: Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer, Integer)
btoa times a b m n
    | b == 0 = (times + 1, a, n, 0)
    | b > 0 && a == m = (times + 1, 0, b, 0)
    | a + b <= m = (times + 1, a + b, 0, 0)
    | a + b > m = (times + 1, m, a + b - m, 0)