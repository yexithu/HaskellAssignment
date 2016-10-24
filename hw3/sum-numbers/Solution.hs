module Solution where
import Data.String
import Data.Char
import Data.Bool

solution :: String -> Int
solution [] = 0
solution x = foldl (+) 0 (map myParseInt (filter isStrNumber (words x)))

myParseInt :: String -> Int
myParseInt [] = 0
myParseInt x = read x :: Int

isStrNumber :: String -> Bool
isStrNumber x = null $ filter (not . isDigit) x