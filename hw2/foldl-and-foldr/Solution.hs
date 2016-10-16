myFoldl :: (a ->b -> a) -> a -> [b] -> a
myFoldl f v [] = v
myFoldl f v x = myFoldl f f v head(x) tail(x)

myFoldr :: (a ->b -> a) -> a -> [b] -> a
myFoldr f v [] = v
myFoldr f v x = myFoldr f f v last(x) init(x)