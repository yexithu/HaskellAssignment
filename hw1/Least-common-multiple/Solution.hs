solution :: Integer -> Integer -> Integer
solution 0 _ = 0
solution _ 0 = 0
solution x y = x * y `div` mygcd x y

mygcd :: Integer -> Integer -> Integer
mygcd x y
    | x `mod` y == 0 = y
    | y `mod` x == 0 = x
    | x <= y = gcd x (y `mod` x)
    | x > y = gcd (x `mod` y) y
